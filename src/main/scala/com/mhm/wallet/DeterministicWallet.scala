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

case class ChangeIndex(change: Int, index: Int)

abstract class DeterministicWallet(gapLimit: Int) {
  def deriveAddresses(rpcCli: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String]
  def getAddresses(rpcCli: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): AddrsSpks = {
    val addrs = deriveAddresses(rpcCli, change, fromIndex, count)
    val spks = addrs.map{ addr =>
      val valAddr: ValidateAddressResult = Await.result(rpcCli.validateAddress(BitcoinAddress(addr)), 20.seconds)
      val spk = valAddr.scriptPubKey.getOrElse(throw new IllegalArgumentException("missing script pub key"))
      spk.hex
    }
    spks.indices.foreach{ index =>
      scriptPubKeyIndex.put(spks(index), ChangeIndex(change, fromIndex + index))
    }
    nextIndex.put(change, Math.max(nextIndex.getOrElse(change, 0), fromIndex+count))
    AddrsSpks(addrs, spks)
  }
  val scriptPubKeyIndex = scala.collection.mutable.Map[String, ChangeIndex]()
  val nextIndex = scala.collection.mutable.Map[Int, Int]()
  def getNewAddresses(rpcCli: BitcoindRpcExtendedClient, change: Int, count: Int): AddrsSpks = {
    getAddresses(rpcCli, change, nextIndex.getOrElse(change, 0), count)
  }
  def rewindOne(change: Int): Unit = {
    nextIndex.put(change, nextIndex.getOrElse(change, 1) - 1)
  }

  /**
   * called when a new tx of ours arrives
   * to see if we need to import more addresses
   */
  def haveScriptpubkeysOverrunGaplimit(scriptpubkeys: Seq[String]): Map[Int, Int] = {
    val result = scala.collection.mutable.Map[Int, Int]()
    scriptpubkeys.foreach { spk =>
      scriptPubKeyIndex.get(spk).map { case ChangeIndex(change, index) =>
        val distanceFromNext = this.nextIndex.getOrElse(change, 0) - index
        if (distanceFromNext <= this.gapLimit)
          result.put(change, Math.max(result.getOrElse(change, 0), this.gapLimit - distanceFromNext + 1))
      }
    }
    result.toMap
  }
}

abstract class DescriptorDeterministicWallet(xpubVbytes: ByteVector, args: XpubDescTempl, gapLimit: Int) extends DeterministicWallet(gapLimit) {
  def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String]
  def obtainDescriptors(rpcCli: BitcoindRpcExtendedClient): Future[Seq[String]] = {
    val dwc = obtainDescriptorsWithoutChecksum(args)
    Future.sequence(dwc.map { d =>
      for {
        descriptorInfo <- rpcCli.getDescriptorInfo(d)
      } yield {
        descriptorInfo.descriptor
      }
    })
  }
  var descriptors: List[String] = Nil
  def deriveAddresses(rpcCli: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String] = {
    val range: Vector[Double] = Vector(fromIndex, fromIndex + count - 1)
    val resultFut = rpcCli.deriveAddresses(descriptors(change), Some(range))
    val result: DeriveAddressesResult = Await.result(resultFut, 20.seconds) // TODO await is ugly, to be removed!!!
    result.addresses.map(_.value)
  }
}

class SingleSigWallet(rpcCli: BitcoindRpcExtendedClient, xpubVbytes: ByteVector, args: XpubDescTempl, gapLimit: Int) extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = {
    val xpub = WalletOps.convertToStandardXpub(args.xpub, xpubVbytes)
    val descriptorsWithoutChecksum = (0 to 1).map{ change =>
      val s = args.descTempl.replaceFirst("\\{change\\}", change.toString)
      s.replaceFirst("\\{xpub\\}",xpub)
    }
    descriptorsWithoutChecksum
  }
  this.descriptors = Await.result(this.obtainDescriptors(rpcCli), 20.seconds).toList // TODO very very ugly!!!!!
}

class MultisigWallet(xpubVbytes: ByteVector, args: XpubDescTempl) extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit = 0){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = ???
}

class SingleSigOldMnemonicWallet extends DeterministicWallet(gapLimit = 0) {
  override def deriveAddresses(rpcCli: BitcoindRpcExtendedClient, change: Int, fromIndex: Int, count: Int): Seq[String] = ???
}

object DeterministicWallet {
  def parseElectrumMasterPublicKey(rpcCli: BitcoindRpcExtendedClient, keyData: String, gapLimit: Int, chain: String): DescriptorDeterministicWallet = {
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
        new SingleSigWallet(rpcCli, xpubVBytes, XpubDescTempl(keyData, descriptorTemplate), gapLimit)
      case None =>
        throw new IllegalArgumentException("SingleSigOldMnemonicWallet not implemented yet")
    }
    wallet
  }
}
