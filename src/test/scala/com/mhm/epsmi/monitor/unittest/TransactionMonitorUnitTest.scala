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
package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.{TransactionMonitorImpl, Tx4HistoryGen}
import com.mhm.epsmi.dummymonitor.DummyBtcRpc.toRpcTransaction
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyTxCreator}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class TransactionMonitorUnitTest extends FlatSpec {

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is not zero" in {
    val BlockHeight     = 133
    val (_, _, dummyTx) = DummyTxCreator.createDummyFundingTx()

    val dummyBtcRpc = DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> BlockHeight))

    val transactionMonitor = new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false)

    val tx  = Tx4HistoryGen(dummyTx.confirmations, dummyTx.txId, Some(DoubleSha256DigestBE.fromHex(dummyTx.blockhash)))
    val txd = toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe BlockHeight
    newHistoryElement.txHash shouldBe dummyTx.txId
  }

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is zero" in {
    val (_, _, dummyTx) = DummyTxCreator.createDummyFundingTx(confirmations = 0)

    val utxoSet     = Seq(dummyTx.vin)
    val dummyBtcRpc = DummyBtcRpc(Seq(dummyTx), utxoSet, Map(dummyTx.blockhash -> 133))

    val transactionMonitor = new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false)

    val tx  = Tx4HistoryGen(dummyTx.confirmations, dummyTx.txId, Some(DoubleSha256DigestBE.fromHex(dummyTx.blockhash)))
    val txd = toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 0
    newHistoryElement.txHash shouldBe dummyTx.txId
    newHistoryElement.fee shouldBe BigDecimal(2)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val OutSpk = "abab"
    val InSpk  = "cdcd"

    val (_, _, inputTx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(InSpk))
    assert(inputTx.vout.scriptPubKey == InSpk)

    val (_, _, tx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(OutSpk), inputTxid = inputTx.txId)
    assert(tx.vout.scriptPubKey == OutSpk)
    assert(tx.vin.txId == inputTx.txId)

    val dummyBtcRpc = DummyBtcRpc(Seq(tx, inputTx))

    val (outputScriptpubkeys, inputScriptpubkeys, tr) =
      new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false)
        .getInputAndOutputScriptpubkeys(DoubleSha256DigestBE.fromHex(tx.txId))
    (outputScriptpubkeys should contain).theSameElementsAs(Seq(OutSpk))
    (inputScriptpubkeys should contain).theSameElementsAs(Seq(InSpk))
    tr.txid.hex shouldBe tx.txId
  }
}
