package com.mhm.epsmi.modules

import com.mhm.bitcoin.{AddressHistory, HistoryEntry, TransactionMonitor}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.connectors.BitcoinSConnector.rpcCli
import com.mhm.integration.epsmi.api.IntTestFixture
import com.mhm.util.HashOps
import com.mhm.wallet.{SingleSigWallet, XpubDescTempl}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}
import scodec.bits.HexStringSyntax

class TransactionMonitorTest extends FlatSpec with IntTestFixture {

  "transaction monitor" should "gather history of transactions" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25)
    val deterministicWallets = Seq(singleSigWallet)

    val monitoredScriptPubKeys = Seq(
      "76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac",
      "76a9143fe7f4ee744d330cbcc8ec5d68925e63ce03f77888ac",
      "76a914a2946db89edc09f56960cee76dab97604f7ffef088ac"
    )
    val addressHistory = TransactionMonitor.buildAddressHistory(BitcoinSConnector.rpcCli, monitoredScriptPubKeys, deterministicWallets)
    monitoredScriptPubKeys.foreach { k =>
      addressHistory.m(HashOps.script2ScriptHash(k)) shouldBe HistoryEntry(subscribed = false, Nil)
    }
    print(addressHistory.m)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val(outputScriptpubkeys, inputScriptpubkeys, tr) = TransactionMonitor.getInputAndOutputScriptpubkeys(
      BitcoinSConnector.rpcCli,
      DoubleSha256DigestBE.fromHex("22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31")
    )
    outputScriptpubkeys should contain theSameElementsAs Seq( // TODO remove these values as they may change
      "76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac",
      "76a914642eef08604841e26f0e9519d51170bd311728df88ac"
    )
    inputScriptpubkeys shouldBe Nil
    tr.txid.hex shouldBe "22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31"
  }

}
