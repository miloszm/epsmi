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

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class TxSpendingFromAddressTest extends FlatSpec {
  "transaction spending FROM one of our addresses" should "cause funding address have both txs in history" in {
    val (dummySpk1, containingBlockHeight1, inputTx)    = createDummyFundingTx()
    val (dummySpk2, containingBlockHeight2, spendingTx) = createDummyFundingTx(inputTxid = inputTx.txId)

    val rpc = DummyBtcRpc(
      Seq(inputTx, spendingTx),
      Nil,
      Map(inputTx.blockhash -> containingBlockHeight1, spendingTx.blockhash -> containingBlockHeight2)
    )

    val monitor      = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
    val sh           = script2ScriptHash(dummySpk1)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 2
  }
}
