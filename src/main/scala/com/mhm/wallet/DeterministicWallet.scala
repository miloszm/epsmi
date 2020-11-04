package com.mhm.wallet

import com.mhm.connectors.BitcoindRpcExtendedClient
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import scodec.bits.{ByteVector, HexStringSyntax}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

case class XpubDescTempl(xpub: String, descTempl: String)

abstract class DeterministicWallet()

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

class SingleSigOldMnemonicWallet extends DeterministicWallet

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
