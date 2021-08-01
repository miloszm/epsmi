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

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorFactory}
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ## tx confirmed with 1 confirmation, then confirmations goes to 100
// ## test that the reorganizable_txes list length goes down

class TxSafeFromReorgTest extends FlatSpec {
  "transaction confirmed beyond reorg threshold" should "be removed from the reorganizable list" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
    val rpc                                           = DummyBtcRpc(Seq(dummyTx1), Nil, Map(dummyTx1.blockhash -> containingBlockHeight1))
    val monitor                                       = TransactionMonitorFactory.create(rpc)
    val monitorState                                  = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
    val (updatedTxs, monitorState2)                   = monitor.checkForUpdatedTxs(monitorState)
    updatedTxs.size shouldBe 0
    monitorState2.reorganizableTxes.size shouldBe 1
    val dummyTx1Unreorganizable      = dummyTx1.copy(confirmations = 2000)
    val rpc2                         = rpc.copy(txList = Seq(dummyTx1Unreorganizable))
    val monitor2                     = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs2, monitorState3) = monitor2.checkForUpdatedTxs(monitorState2)
    updatedTxs2.size shouldBe 0
    monitorState3.reorganizableTxes.size shouldBe 0
  }
}
