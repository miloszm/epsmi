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
import com.mhm.util.HashOps
import org.scalatest.FlatSpec

//  ### build empty address history, subscribe one address
//  ### an unconfirmed tx appears, then confirms

class TxArrivalThenConfirmationTest extends FlatSpec with AddressHistoryAssertions {

  "an unconfirmed tx appears for an address, then confirms" should "update tx height in history for this address" in {
    val (dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)

    val rpc          = DummyBtcRpc(Nil, Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
    val monitor      = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1

    val sh = HashOps.script2ScriptHash(dummySpk)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

    val monitorState2 = monitorState.subscribeAddress(sh)

    val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
    updatedTxs.isEmpty shouldBe true

    // unconfirmed transaction appears
    val rpc2                         = rpc.copy(txList = Seq(dummyTx))
    val monitor2                     = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs2, monitorState4) = monitor2.checkForUpdatedTxs(monitorState3)
    updatedTxs2.size shouldBe 1
    monitorState4.unconfirmedTxes.size shouldBe 1
    monitorState4.unconfirmedTxes.keys.head shouldBe dummyTx.txId
    assertAddressHistoryTx(
      monitorState4.addressHistory,
      spk        = dummySpk,
      height     = 0,
      txId       = dummyTx.txId,
      subscribed = true
    )

    // transaction confirms
    val dummyTxConfirmed   = dummyTx.copy(confirmations = 1)
    val rpc3               = rpc2.copy(txList = Seq(dummyTxConfirmed))
    val monitor3           = TransactionMonitorFactory.create(rpc3)
    val (_, monitorState5) = monitor3.checkForUpdatedTxs(monitorState4)
    assertAddressHistoryTx(
      monitorState5.addressHistory,
      spk        = dummySpk,
      height     = containingBlockHeight,
      txId       = dummyTx.txId,
      subscribed = true
    )
  }

}
