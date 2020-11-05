package com.mhm.wallet

import com.mhm.connectors.BitcoindRpcExtendedClient
import org.bitcoins.commons.jsonmodels.bitcoind.{DeriveAddressesResult, ValidateAddressResult}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

case class XpubDescTempl(xpub: String, descTempl: String)

case class AddrsSpks(addrs: Seq[String], spks: Seq[String])

trait DeterministicWallet {
  def deriveAddresses(rpcCliExt: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String]
  def getAddresses(rpcCli: BitcoindRpcClient, rpcCliExt: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): AddrsSpks = {
    val addrs = deriveAddresses(rpcCliExt, change, fromIndex, count)
    val spks = addrs.map{ addr =>
      val valAddr: ValidateAddressResult = Await.result(rpcCli.validateAddress(BitcoinAddress(addr)), 20.seconds)
      val spk = valAddr.scriptPubKey.getOrElse(throw new IllegalArgumentException("missing script pub key"))
      spk.hex
    }
    spks.indices.foreach{ index =>
      scriptPubKeyIndex.put(spks(index), Seq(change, fromIndex + index))
    }
    nextIndex.put(change, Math.max(nextIndex.getOrElse(change, 0), fromIndex+count))
    AddrsSpks(addrs, spks)
  }
  val scriptPubKeyIndex = scala.collection.mutable.Map[String, Seq[Int]]()
  val nextIndex = scala.collection.mutable.Map[Int, Int]()
  def getNewAddresses(rpcCli: BitcoindRpcClient, rpcCliExt: BitcoindRpcExtendedClient, change: Int, count: Int): AddrsSpks = {
    getAddresses(rpcCli, rpcCliExt, change, nextIndex.getOrElse(change, 0), count)
  }
  def rewindOne(change: Int): Unit = {
    nextIndex.put(change, nextIndex.getOrElse(change, 1) - 1)
  }
}

abstract class DescriptorDeterministicWallet(xpubVbytes: ByteVector, args: XpubDescTempl) extends DeterministicWallet() {
  def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String]
  def obtainDescriptors(rpcCliExt: BitcoindRpcExtendedClient): Future[Seq[String]] = {
    val dwc = obtainDescriptorsWithoutChecksum(args)
    Future.sequence(dwc.map { d =>
      for {
        descriptorInfo <- rpcCliExt.getDescriptorInfo(d)
      } yield {
        descriptorInfo.descriptor
      }
    })
  }
  var descriptors: List[String] = Nil
  def deriveAddresses(rpcCliExt: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String] = {
    val range: Vector[Double] = Vector(fromIndex, fromIndex + count - 1)
    val resultFut = rpcCliExt.deriveAddresses(descriptors(change), Some(range))
    val result: DeriveAddressesResult = Await.result(resultFut, 20.seconds) // TODO await is ugly, to be removed!!!
    result.addresses.map(_.value)
  }
}

class SingleSigWallet(rpcCliExt: BitcoindRpcExtendedClient, xpubVbytes: ByteVector, args: XpubDescTempl) extends DescriptorDeterministicWallet(xpubVbytes, args){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = {
    val xpub = WalletUtil.convertToStandardXpub(args.xpub, xpubVbytes)
    val descriptorsWithoutChecksum = (0 to 1).map{ change =>
      val s = args.descTempl.replaceFirst("\\{change\\}", change.toString)
      s.replaceFirst("\\{xpub\\}",xpub)
    }
    descriptorsWithoutChecksum
  }
  this.descriptors = Await.result(this.obtainDescriptors(rpcCliExt), 20.seconds).toList // TODO very very ugly!!!!!
}

class MultisigWallet(xpubVbytes: ByteVector, args: XpubDescTempl) extends DescriptorDeterministicWallet(xpubVbytes, args){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = ???
}

class SingleSigOldMnemonicWallet extends DeterministicWallet {
  override def deriveAddresses(rpcCliExt: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String] = ???
}

object DeterministicWallet {
  def parseElectrumMasterPublicKey(rpcCliExt: BitcoindRpcExtendedClient, keyData: String, gapLimit: Int, chain: String): DescriptorDeterministicWallet = {
    val xpubVBytes: ByteVector = if (chain == "main") hex"0488b21e"
      else if (chain == "test" || chain == "regtest") hex"043587cf"
      else throw new IllegalStateException("unrecognized bitcoin chain")

    val descriptorTemplateOpt: Option[String] = if (Set("xpub", "tpub").contains(keyData.take(4))) Some("pkh({xpub}/{change}/*)")
      else if (Set("zpub", "vpub").contains(keyData.take(4))) Some("wpkh({xpub}/{change}/*)")
      else if (Set("ypub", "upub").contains(keyData.take(4))) Some("sh(wpkh({xpub}/{change}/*))")
      else None

    if (keyData.contains(" "))
      throw new IllegalArgumentException("multisig not implemented yet")

    val wallet = descriptorTemplateOpt match {
      case Some(descriptorTemplate) =>
        val w = new SingleSigWallet(rpcCliExt, xpubVBytes, XpubDescTempl(keyData, descriptorTemplate))
        w
        // todo init descriptors inside SingleSigWallet
      case None =>
        throw new IllegalArgumentException("SingleSigOldMnemonicWallet not implemented yet")
    }
    //SingleSigWallet(Nil, gapLimit, Nil, Map(), xpubVBytes)
    // new SingleSigWallet(xpubVBytes, null)
    wallet
  }
}
