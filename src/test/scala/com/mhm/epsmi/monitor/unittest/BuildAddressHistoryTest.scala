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
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet, DummyTxCreator}
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

class BuildAddressHistoryTest extends FlatSpec with AddressHistoryAssertions {

  "transaction monitor" should "build single entry address history" in {
    val (dummySpk, blockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx()
    val rpc                              = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> blockHeight))
    val monitor                          = TransactionMonitorFactory.create(rpc)
    val state                            = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    state.addressHistory.m.size shouldBe 1
    assertAddressHistoryTx(state.addressHistory, dummySpk, blockHeight, dummyTx.txId, subscribed = false)
  }

  "transaction monitor" should " build address history with two entries" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = DummyTxCreator.createDummyFundingTx()
    val (dummySpk2, containingBlockHeight2, dummyTx2) = DummyTxCreator.createDummyFundingTx()
    val rpc = new DummyBtcRpc(
      Seq(dummyTx1, dummyTx2),
      Nil,
      Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2)
    )
    val monitor = TransactionMonitorFactory.create(rpc)
    val state   = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))
    state.addressHistory.m.size shouldBe 2
    assertAddressHistoryTx(state.addressHistory, dummySpk1, containingBlockHeight1, dummyTx1.txId, subscribed = false)
    assertAddressHistoryTx(state.addressHistory, dummySpk2, containingBlockHeight2, dummyTx2.txId, subscribed = false)
  }

  "transaction monitor" should " build address history with many entries" in {
    val (inputSpk, inputBlockHeight1, inputTx) = DummyTxCreator.createDummyFundingTx()
    val (dummySpk, containingBlockHeight, dummyTx) =
      DummyTxCreator.createDummyFundingTx(confirmations = 0, inputTxid = inputTx.vin.txId)
    val sh = HashOps.script2ScriptHash(dummySpk)

    val InitialTxCount = 1100 // we want to exceed the batch size of 1000
    val txs1           = Seq(dummyTx)
    val txs2 = for (_ <- 0 until InitialTxCount - 1) yield {
      val (_, _, tx) = DummyTxCreator
        .createDummyFundingTx(outputSpkOpt = Some(dummySpk), inputTxid = inputTx.vin.txId, confirmations = 0)
      tx
    }
    val txs = txs1 ++ txs2
    txs.length shouldBe InitialTxCount

    val rpc     = DummyBtcRpc(txs, Seq(dummyTx.vin))
    val monitor = TransactionMonitorFactory.create(rpc)
    val state   = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    assert(state.lastKnownTx.isDefined)
    state.addressHistory.m.size shouldBe 1
    state.addressHistory.m(sh).history.length shouldBe InitialTxCount

    val (updatedScripthashes, state1) = monitor.checkForUpdatedTxs(state)
    updatedScripthashes.size shouldBe 0
    state1.addressHistory.m(sh).history.length shouldBe InitialTxCount

    val AddedTxCount = 130
    val newTxs = for (_ <- 0 until AddedTxCount) yield {
      val (_, _, tx) = DummyTxCreator
        .createDummyFundingTx(outputSpkOpt = Some(dummySpk), inputTxid = inputTx.vin.txId, confirmations = 0)
      tx
    }

    val newRpc                         = rpc.copy(txList = rpc.txList ++ newTxs)
    val newMonitor                     = TransactionMonitorFactory.create(newRpc)
    val (updatedScripthashes2, state2) = newMonitor.checkForUpdatedTxs(state1)
    updatedScripthashes2.size shouldBe 0
    state2.addressHistory.m(sh).history.length shouldBe InitialTxCount + AddedTxCount
  }

//  "transaction monitor" should "have checking for new transactions functionality" in {
//    val(dummySpk1, containingBlockHeight1, dummyTx1) = DummyTxCreator.createDummyFundingTx()
//    val(dummySpk2, containingBlockHeight2, dummyTx2) = DummyTxCreator.createDummyFundingTx()
//    val(dummySpk3, containingBlockHeight3, dummyTx3) = DummyTxCreator.createDummyFundingTx() // most recent initially
//    val(dummySpk4, containingBlockHeight4, dummyTx4) = DummyTxCreator.createDummyFundingTx()
//    val(dummySpk5, containingBlockHeight5, dummyTx5) = DummyTxCreator.createDummyFundingTx()
//
//    val rpc = DummyBtcRpc(Seq(dummyTx1, dummyTx2, dummyTx3),
//      Nil,
//      Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2, dummyTx3.blockhash -> containingBlockHeight3))
//
//    val monitor = TransactionMonitorFactory.create(rpc)
//    val state = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2, dummySpk3, dummySpk4, dummySpk5), Seq(new DummyDeterministicWallet))
//
//    val (_, stateAfterCheck) = monitor.checkForUpdatedTxs(state)
//    assert(state.lastKnownTx == stateAfterCheck.lastKnownTx)
//    assert(state.addressHistory == stateAfterCheck.addressHistory)
//
//    val rpc2 = DummyBtcRpc(Seq(dummyTx1, dummyTx2, dummyTx3, dummyTx4, dummyTx5),
//      Nil,
//      Map(
//        dummyTx1.blockhash -> containingBlockHeight1,
//        dummyTx2.blockhash -> containingBlockHeight2,
//        dummyTx3.blockhash -> containingBlockHeight3,
//        dummyTx4.blockhash -> containingBlockHeight4,
//        dummyTx5.blockhash -> containingBlockHeight5
//      )
//    )
//
//    val monitor2 = TransactionMonitorFactory.create(rpc2)
//
//    val (_, state2) = monitor2.checkForUpdatedTxs(state)
//    state2.updatedScripthashes should contain theSameElementsAs Seq(script2ScriptHash(dummySpk4), script2ScriptHash(dummySpk5))
//
//    val (_, state3) = monitor2.checkForUpdatedTxs(state2)
//    println(state3.updatedScripthashes)
//    state3.updatedScripthashes.isEmpty shouldBe true
//  }

}
