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
import org.scalatest.FlatSpec

//  ### one unconfirmed tx in wallet belonging to us, with confirmed inputs,
//  ### addr history built, then tx confirms, not subscribed to address

class NonSubscribedConfirmationTest extends FlatSpec with AddressHistoryAssertions {

  "confirming transaction for unsubscribed address" should "not appear as updated when confirming" in {
    val (dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)

    val rpc     = DummyBtcRpc(Seq(dummyTx), Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
    val monitor = TransactionMonitorFactory.create(rpc)

    val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1

    assertAddressHistoryTx(
      monitorState.addressHistory,
      spk        = dummySpk,
      height     = 0,
      txId       = dummyTx.txId,
      subscribed = false
    )

    val (updatedTxs, monitorState2) = monitor.checkForUpdatedTxs(monitorState)
    monitorState2.unconfirmedTxes.size shouldBe 1
    updatedTxs.size shouldBe 0

    val dummyTxConfirmed             = dummyTx.copy(confirmations = 1) // tx confirms
    val rpc2                         = rpc.copy(txList = Seq(dummyTxConfirmed))
    val monitor2                     = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs2, monitorState3) = monitor2.checkForUpdatedTxs(monitorState2)
    // #not subscribed so still only returns an empty list
    updatedTxs2.size shouldBe 0
    monitorState3.reorganizableTxes.size shouldBe 1
    assertAddressHistoryTx(
      monitorState3.addressHistory,
      spk        = dummySpk,
      height     = containingBlockHeight,
      txId       = dummyTx.txId,
      subscribed = false
    )
  }
}
