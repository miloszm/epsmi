package com.mhm.wallet

import com.mhm.connectors.RpcWrap.wrap
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{DeriveAddressesResult, ValidateAddressResult}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{DescriptorRpc, UtilRpc}
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

case class XpubDescTempl(xpub: String, descTempl: String)

case class AddrsSpks(addrs: Seq[String], spks: Seq[String])

case class ChangeIndex(change: Int, index: Int)

abstract class DeterministicWallet(gapLimit: Int, val walletName: String) extends Logging {
  def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String]
  def getAddresses(rpcCli: DescriptorRpc with UtilRpc, change: Int, fromIndex: Int, count: Int): AddrsSpks = {
    val addrs = deriveAddresses(rpcCli, change, fromIndex, count)
    val spks = addrs.map{ addr =>
      val valAddr: ValidateAddressResult = wrap(rpcCli.validateAddress(BitcoinAddress(addr)))
      val spk = valAddr.scriptPubKey.getOrElse(throw new IllegalArgumentException("missing script pub key"))
      spk.hex
    }
    logger.info(s"converted ${addrs.size} addresses into ${spks.size} scriptpubkeys, head is ${spks.headOption}")
    spks.indices.foreach{ index =>
      scriptPubKeyIndex.put(spks(index), ChangeIndex(change, fromIndex + index))
    }
    nextIndex.put(change, Math.max(nextIndex.getOrElse(change, 0), fromIndex+count))
    AddrsSpks(addrs, spks)
  }
  val scriptPubKeyIndex = scala.collection.mutable.Map[String, ChangeIndex]()
  val nextIndex = scala.collection.mutable.Map[Int, Int]()
  def getNewAddresses(rpcCli: DescriptorRpc with UtilRpc, change: Int, count: Int): AddrsSpks = {
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

abstract class DescriptorDeterministicWallet(xpubVbytes: ByteVector, args: XpubDescTempl, gapLimit: Int, walletName: String) extends DeterministicWallet(gapLimit, walletName) {
  def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String]
  def obtainDescriptors(rpcCli: DescriptorRpc): Future[Seq[String]] = {
    val dwc = obtainDescriptorsWithoutChecksum(args)
    Future.sequence(dwc.map { d =>
      logger.info(s"getting descriptor info for: $d")
      for {
        descriptorInfo <- rpcCli.getDescriptorInfo(d)
      } yield {
        logger.info(s"descriptor=${descriptorInfo.descriptor}")
        descriptorInfo.descriptor
      }
    })
  }
  var descriptors: List[String] = Nil
  def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String] = {
    val range: Vector[Double] = Vector(fromIndex, fromIndex + count - 1)
    val result: DeriveAddressesResult = wrap(rpcCli.deriveAddresses(descriptors(change), Some(range)), "deriveAddresses") // TODO await is ugly, to be removed!!!
    logger.info(s"derived ${result.addresses.size} addresses from descriptors, head is ${result.addresses.headOption}")
    result.addresses.map(_.value)
  }
}

class SingleSigWallet(rpcCli: DescriptorRpc, xpubVbytes: ByteVector, args: XpubDescTempl, gapLimit: Int, walletName: String) extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit, walletName){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = {
    val xpub = WalletOps.convertToStandardXpub(args.xpub, xpubVbytes)
    val descriptorsWithoutChecksum = (0 to 1).map{ change =>
      val s = args.descTempl.replaceFirst("\\{change\\}", change.toString)
      s.replaceFirst("\\{xpub\\}",xpub)
    }
    descriptorsWithoutChecksum
  }
  this.descriptors = wrap(this.obtainDescriptors(rpcCli)).toList // TODO very very ugly!!!!!
  override def toString: String = {
    s"SingleSigWallet(xpubVbytes=${xpubVbytes.toHex} xpub=${args.xpub} descTemp=${args.descTempl} gapLimit=$gapLimit descriptors=${descriptors.mkString("|")})"
  }
}

class MultisigWallet(xpubVbytes: ByteVector, args: XpubDescTempl, walletName: String) extends DescriptorDeterministicWallet(xpubVbytes, args, gapLimit = 0, walletName){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Seq[String] = ???
}

class SingleSigOldMnemonicWallet extends DeterministicWallet(gapLimit = 0, walletName = "") {
  override def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String] = ???
}

object DeterministicWallet {
  def parseElectrumMasterPublicKey(rpcCli: DescriptorRpc, keyData: String, gapLimit: Int, chain: String, walletName: String): DescriptorDeterministicWallet = {
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
        new SingleSigWallet(rpcCli, xpubVBytes, XpubDescTempl(keyData, descriptorTemplate), gapLimit, walletName)
      case None =>
        throw new IllegalArgumentException("SingleSigOldMnemonicWallet not implemented yet")
    }
    wallet
  }
}
