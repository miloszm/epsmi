package com.mhm.livewallet.test

import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.rpcCli
import com.mhm.wallet.{
  ChangeIndexPair,
  DeterministicWallet,
  DeterministicWalletState,
  SingleSigWallet,
  XpubDescTemplPair
}
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper, have}
import scodec.bits.HexStringSyntax

import scala.jdk.CollectionConverters.SetHasAsScala

/**
  * We assume one master public key is available and valid
  */
class DeterministicWalletTest extends FlatSpec {
  val config          = ConfigFactory.load
  val isTestnet       = config.getBoolean("epsmi.testnet")
  val mpkConfig       = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
  val masterPublicKey = mpkConfig.entrySet().asScala.headOption.getOrElse(fail).getValue.unwrapped().toString

  "parseElectrumMasterPublicKey" should "return wallet" in {
    val wallet = DeterministicWallet
      .parseElectrumMasterPublicKey(rpcCli, keyData = masterPublicKey, gapLimit = 25, chain = "main", "wallet-1")
    wallet.descriptors.nonEmpty shouldBe true
    (wallet.descriptors.map(_.dropRight(8) + "<checksum>") should contain)
      .theSameElementsAs(List(s"pkh($masterPublicKey/0/*)#<checksum>", s"pkh($masterPublicKey/1/*)#<checksum>"))
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors without checksum" in {
    val args                       = XpubDescTemplPair(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet            = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val descriptorsWithoutChecksum = singleSigWallet.obtainDescriptorsWithoutChecksum(args)
    (descriptorsWithoutChecksum should contain).theSameElementsAs(
      List(s"pkh($masterPublicKey/0/*)", s"pkh($masterPublicKey/1/*)")
    )
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors" in {
    val args            = XpubDescTemplPair(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    singleSigWallet.descriptors.nonEmpty shouldBe true
    (singleSigWallet.descriptors.map(_.dropRight(8) + "<checksum>") should contain)
      .theSameElementsAs(List(s"pkh($masterPublicKey/0/*)#<checksum>", s"pkh($masterPublicKey/1/*)#<checksum>"))
  }

  "SingleSigWallet" should "have derive addresses functionality" in {
    val args                   = XpubDescTemplPair(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet        = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val addresses: Seq[String] = singleSigWallet.deriveAddresses(rpcCli, change = 0, fromIndex = 0, count = 3)
    addresses.size shouldBe 3
  }

  "DeterministicWallet" should "have functionality for getting addresses" in {
    val args            = XpubDescTemplPair(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val result          = singleSigWallet.getAddresses(rpcCli, 0, 0, 2)
    (result.addrs should have).size(2)
    (result.spks should have).size(2)
    println(result)
  }

  "DeterministicWallet" should "detect if scriptpubkeys have overrun gap limit" in {
    val GapLimit        = 25
    val args            = XpubDescTemplPair(masterPublicKey, "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = GapLimit, "wallet-1")
    val state = DeterministicWalletState(
      Map("spk1" -> ChangeIndexPair(0, 5), "spk2" -> ChangeIndexPair(1, 7), "spk3" -> ChangeIndexPair(1, 9)),
      Map(0 -> 20, 1 -> 25)
    )
    singleSigWallet.currentState.set(state)
    val result = singleSigWallet.haveScriptpubkeysOverrunGaplimit(Seq("spk1", "spk2", "spk3"))
    (result.keys should contain).theSameElementsAs(Seq(0, 1))
    result(0) shouldBe (GapLimit - (20 - 5) + 1)
    result(1) shouldBe (GapLimit - (25 - 9) + 1)
  }

}
