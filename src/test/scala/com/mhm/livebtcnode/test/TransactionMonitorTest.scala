/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.livebtcnode.test

import com.mhm.bitcoin.{TransactionMonitorFactory, TransactionMonitorImpl, Tx4HistoryGen}
import com.mhm.common.model.{HistoryElement, HistoryEntry}
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.rpcCli
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.util.HashOps
import com.mhm.wallet.{SingleSigWallet, XpubDescTemplPair}
import org.bitcoins.commons.jsonmodels.bitcoind.GetRawTransactionResult
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}
import scodec.bits.HexStringSyntax

class TransactionMonitorTest extends FlatSpec {

  "transaction monitor" should "build history of transactions" in {
    val args = XpubDescTemplPair(
      "xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj",
      "pkh({xpub}/{change}/*)"
    )
    val singleSigWallet      = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val deterministicWallets = Seq(singleSigWallet)

    val monitoredScriptPubKeys = Seq(
      "76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac",
      "76a9143fe7f4ee744d330cbcc8ec5d68925e63ce03f77888ac",
      "76a914a2946db89edc09f56960cee76dab97604f7ffef088ac"
    )
    val buildResult = TransactionMonitorFactory
      .create(TestBitcoinSConnector.rpcCli)
      .buildAddressHistory(monitoredScriptPubKeys, deterministicWallets)
    monitoredScriptPubKeys.take(1).foreach { k =>
      buildResult.addressHistory.m(HashOps.script2ScriptHash(k)) shouldBe HistoryEntry(
        subscribed = false,
        List(HistoryElement("22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31", 654929, 0))
      )
    }
    monitoredScriptPubKeys.drop(1).foreach { k =>
      buildResult.addressHistory.m(HashOps.script2ScriptHash(k)) shouldBe HistoryEntry(subscribed = false, Nil)
    }
    print(buildResult.addressHistory.m)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val (outputScriptpubkeys, inputScriptpubkeys, tr) =
      new TransactionMonitorImpl(TestBitcoinSConnector.rpcCli, nonWalletAllowed = true).getInputAndOutputScriptpubkeys(
        DoubleSha256DigestBE.fromHex("22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31")
      )
    (outputScriptpubkeys should contain).theSameElementsAs(
      Seq("76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac", "76a914642eef08604841e26f0e9519d51170bd311728df88ac")
    )
    (inputScriptpubkeys should contain).theSameElementsAs(Seq("76a914d5a77c5d45adf40f610d1fe600a437a80649815b88ac"))
    tr.txid.hex shouldBe "22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31"
  }

  "transaction monitor" should "have functionality for generating new history element" in {
    val transactionMonitor = new TransactionMonitorImpl(TestBitcoinSConnector.rpcCli, nonWalletAllowed = true)

    val txid                           = "22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31"
    val rawTx: GetRawTransactionResult = wrap(rpcCli.getRawTransaction(DoubleSha256DigestBE.fromHex(txid)))
    val tx                             = Tx4HistoryGen(rawTx.confirmations.getOrElse(fail), txid, rawTx.blockhash)
    val txd                            = wrap(rpcCli.decodeRawTransaction(rawTx.hex))

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 654929
    newHistoryElement.txHash shouldBe "22667c482f0f69daefabdf0969be53b8d539e1d2abbfc1c7a193ae38ec0d3e31"
  }

}
