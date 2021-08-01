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
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// # a tx is confirmed, a chain reorganization happens and that tx is replaced
// # by another tx spending the same input, the original tx is now conflicted

class ReorgRaceAttackTest extends FlatSpec with AddressHistoryAssertions {
  "tx1 becomes reorged and conflicted, tx2 that has tx1 as input, confirms" should "remove tx1 from history, add tx2 to history with new height" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
    val (dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(inputTxid = dummyTx1.vin.txId)

    val rpc = DummyBtcRpc(
      Seq(dummyTx1),
      Nil,
      Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2)
    )

    val monitor      = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))

    monitorState.addressHistory.m.size shouldBe 2
    val sh1 = script2ScriptHash(dummySpk1)
    val sh2 = script2ScriptHash(dummySpk2)
    monitorState.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 1
    monitorState.getElectrumHistory(sh2).getOrElse(fail).size shouldBe 0
    assertAddressHistoryTx(
      monitorState.addressHistory,
      spk        = dummySpk1,
      height     = containingBlockHeight1,
      txId       = dummyTx1.txId,
      subscribed = false
    )

    //  #race attack happens
    //  #dummy_tx1 goes to -1 confirmations, dummy_tx2 gets confirmed
    val dummyTx1Conflicted = dummyTx1.copy(confirmations = -1)
    val dummyTx2Confirmed  = dummyTx2.copy(confirmations = 1)
    val rpc2               = rpc.copy(txList             = Seq(dummyTx1Conflicted, dummyTx2Confirmed))

    val monitor2                    = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    updatedTxs.size shouldBe 0
    monitorState2.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 0
    monitorState2.getElectrumHistory(sh2).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(
      monitorState2.addressHistory,
      spk        = dummySpk2,
      height     = containingBlockHeight2,
      txId       = dummyTx2.txId,
      subscribed = false
    )
  }
}
