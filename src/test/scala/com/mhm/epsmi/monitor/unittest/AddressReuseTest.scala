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

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorFactory, TransactionMonitorImpl}
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class AddressReuseTest extends FlatSpec {
  val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val (dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(outputSpkOpt = Some(dummySpk1))

  val rpc = DummyBtcRpc(
    Seq(dummyTx1),
    Nil,
    Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2)
  )

  val monitor = TransactionMonitorFactory.create(rpc)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  val sh           = script2ScriptHash(dummySpk1)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1

  "transaction arriving to an address with nonempty transaction history" should "be added to this address' history" in {
    val rpc2     = rpc.copy(txList = rpc.txList ++ Seq(dummyTx2))
    val monitor2 = TransactionMonitorFactory.create(rpc2)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 2
  }
}
