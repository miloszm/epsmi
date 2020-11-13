package com.mhm.livewallet.test

import com.mhm.connectors.BitcoinSConnector.rpcCli
import com.mhm.wallet.{DeterministicWallet, SingleSigWallet, XpubDescTempl}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper, have}
import scodec.bits.HexStringSyntax

class DeterministicWalletTest extends FlatSpec {

  "parseElectrumMasterPublicKey" should "return wallet" in {
    val wallet = DeterministicWallet.parseElectrumMasterPublicKey(
      rpcCli,
      keyData = "xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj",
      gapLimit = 25,
      chain = "main",
      "walllet-1"
    )
    wallet.descriptors.isEmpty shouldBe false
    println(wallet.descriptors)
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors without checksum" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val descriptorsWithoutChecksum = singleSigWallet.obtainDescriptorsWithoutChecksum(args)
    descriptorsWithoutChecksum should contain theSameElementsAs List (
      "pkh(xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj/0/*)",
      "pkh(xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj/1/*)"
    )
  }

  "SingleSigWallet" should "have functionality for obtaining descriptors" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    singleSigWallet.descriptors.isEmpty shouldBe false
    println(singleSigWallet.descriptors)
  }

  "SingleSigWallet" should "have derive addresses functionality" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val addresses: Seq[String] = singleSigWallet.deriveAddresses(rpcCli, change = 0, fromIndex = 0, count = 2)
    addresses.isEmpty shouldBe false
    println(addresses)
  }

  "DeterministicWallet" should "have functionality for getting addresses" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
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
