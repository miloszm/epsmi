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

class UnrelatedTxTest extends FlatSpec with AddressHistoryAssertions {

  "transaction that has nothing to do with our wallet" should "not enter history" in {
    val (_, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)
    val ourDummySpk                         = "beef" * 16

    val rpc     = DummyBtcRpc(Seq(dummyTx), Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
    val monitor = TransactionMonitorFactory.create(rpc)

    val monitorState = monitor.buildAddressHistory(Seq(ourDummySpk), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1
    assertHistoryEmpty(monitorState.addressHistory, ourDummySpk)
  }

}
