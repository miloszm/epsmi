package com.mhm.livewallet.test

import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.rpcCli
import com.mhm.wallet.{DeterministicWallet, SingleSigWallet, XpubDescTempl}
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper, have}
import scodec.bits.HexStringSyntax

import scala.jdk.CollectionConverters.SetHasAsScala

/**
 * We assume one master public key is available and valid
 */
class DeterministicWalletTest extends FlatSpec {
  val config = ConfigFactory.load
  val isTestnet = config.getBoolean("epsmi.testnet")
  val mpkConfig = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
  val masterPublicKey = mpkConfig.entrySet().asScala.headOption.getOrElse(fail).getValue.unwrapped().toString

  "parseElectrumMasterPublicKey" should "return wallet" in {
    val wallet = DeterministicWallet.parseElectrumMasterPublicKey(
      rpcCli,
      keyData = masterPublicKey,
      gapLimit = 25,
      chain = "main",
      "wallet-1"
    )
    wallet.descriptors.nonEmpty shouldBe true
    wallet.descriptors.map(_.dropRight(8) + "<checksum>") should contain theSameElementsAs List (
      s"pkh($masterPublicKey/0/*)#<checksum>",
      s"pkh($masterPublicKey/1/*)#<checksum>"
    )
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors without checksum" in {
    val args = XpubDescTempl(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val descriptorsWithoutChecksum = singleSigWallet.obtainDescriptorsWithoutChecksum(args)
    descriptorsWithoutChecksum should contain theSameElementsAs List (
      s"pkh($masterPublicKey/0/*)",
      s"pkh($masterPublicKey/1/*)"
    )
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors" in {
    val args = XpubDescTempl(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    singleSigWallet.descriptors.nonEmpty shouldBe true
    singleSigWallet.descriptors.map(_.dropRight(8) + "<checksum>") should contain theSameElementsAs List (
      s"pkh($masterPublicKey/0/*)#<checksum>",
      s"pkh($masterPublicKey/1/*)#<checksum>"
    )
  }

  "SingleSigWallet" should "have derive addresses functionality" in {
    val args = XpubDescTempl(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val addresses: Seq[String] = singleSigWallet.deriveAddresses(rpcCli, change = 0, fromIndex = 0, count = 3)
    addresses.size shouldBe 3
  }

  "DeterministicWallet" should "have functionality for getting addresses" in {
    val args = XpubDescTempl(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val result = singleSigWallet.getAddresses(rpcCli, 0, 0, 2)
    result.addrs should have size 2
    result.spks should have size 2
    println(result)
  }

  "DeterministicWallet" should "detect if scriptpubkeys have overrun gap limit" in {

    //in:
    //['76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac', '76a914642eef08604841e26f0e9519d51170bd311728df88ac']
    //out:
    //
  }

}
