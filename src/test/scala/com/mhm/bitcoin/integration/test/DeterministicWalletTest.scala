package com.mhm.bitcoin.integration.test

import com.mhm.connectors.BitcoinSConnector.{rpcCli, rpcCliExt}
import com.mhm.wallet.{DeterministicWallet, SingleSigWallet, XpubDescTempl}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper, have}
import scodec.bits.HexStringSyntax

class DeterministicWalletTest extends FlatSpec with IntTestFixture {

  "parseElectrumMasterPublicKey" should "return wallet" in {
    val wallet = DeterministicWallet.parseElectrumMasterPublicKey(
      rpcCliExt,
      keyData = "xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj",
      gapLimit = 25,
      chain = "main"
    )
    wallet.descriptors.isEmpty shouldBe false
    println(wallet.descriptors)
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors without checksum" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCliExt, hex"0488b21e", args)
    val descriptorsWithoutChecksum = singleSigWallet.obtainDescriptorsWithoutChecksum(args)
    descriptorsWithoutChecksum should contain theSameElementsAs List (
      "pkh(xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj/0/*)",
      "pkh(xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj/1/*)"
    )
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCliExt, hex"0488b21e", args)
    singleSigWallet.descriptors.isEmpty shouldBe false
    println(singleSigWallet.descriptors)
  }

  "SingleSigWallet" should "have derive addresses functionality" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCliExt, hex"0488b21e", args)
    val addresses: Seq[String] = singleSigWallet.deriveAddresses(rpcCliExt, change = 0, fromIndex = 0, count = 2)
    addresses.isEmpty shouldBe false
    println(addresses)
  }

  "DeterministicWallet" should "have functionality for getting addresses" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCliExt, hex"0488b21e", args)
    val result = singleSigWallet.getAddresses(rpcCli, rpcCliExt, 0, 0, 2)
    result.addrs should have size 2
    result.spks should have size 2
    println(result)
  }

}
