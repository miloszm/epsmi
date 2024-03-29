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
package com.mhm.epsmi.roundtrip.test

import java.io.ByteArrayOutputStream

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.common.model.{HistoryElement, HistoryEntry}
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.DummyDeterministicWallet
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.ec

class HeartbeatRoundtripSpec extends FlatSpec {

  "heartbeat connected" should "trigger processing and eventually notifications about a new transaction" in {

    val (dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)
    val sh                                         = HashOps.script2ScriptHash(dummySpk)
    val rpc                                        = DummyRoundtripBtcRpc(Nil, Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
    val monitor                                    = TransactionMonitorFactory.create(rpc)
    val monitorState                               = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1
    monitorState.addressHistory.m shouldBe Map(sh -> HistoryEntry(subscribed = false, List()))
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

    println(s"subscribing address: $sh (derived from spk: $dummySpk)")
    val monitorState2 = monitorState.subscribeAddress(sh)
    monitorState2.addressHistory.m shouldBe Map(sh -> HistoryEntry(subscribed = true, List()))

    val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
    updatedTxs.isEmpty shouldBe true
    monitorState3.updatedScripthashes shouldBe Nil

    val rpc2     = rpc.copy(txList = Seq(dummyTx))
    val monitor2 = TransactionMonitorFactory.create(rpc2)

    val protocol = new Api4ElectrumImpl(Api4ElectrumCore(rpc2), monitor2, monitorState3)

    val streamOutput = new ByteArrayOutputStream()

    protocol.triggerHeartbeatConnected(streamOutput)

    protocol.currentMonitorState.get().getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    protocol.currentMonitorState.get().addressHistory.m shouldBe Map(
      sh -> HistoryEntry(subscribed = true, List(HistoryElement(dummyTx.txId, 0, 2)))
    )
    val output              = streamOutput.toString
    val expectedHistoryHash = protocol.currentMonitorState.get.getElectrumHistoryHash(sh)
    output shouldBe s"""{"jsonrpc": "2.0", "method": "blockchain.scripthash.subscribe", "params": ["$sh", "$expectedHistoryHash"]}""" + "\n"

    val monitorState4 = monitorState3.subscribeToHeaders(true)
    val protocol2     = new Api4ElectrumImpl(Api4ElectrumCore(rpc2), monitor2, monitorState4)
    val streamOutput2 = new ByteArrayOutputStream()
    protocol2.triggerHeartbeatConnected(streamOutput2)
    val output2 = streamOutput2.toString

    val expectedBestBlockHashHeight = wrap(Api4ElectrumCore(rpc2).getBlockHeaderRaw(wrap(rpc2.getBestBlockHash)))
    val expected =
      s"""{"jsonrpc": "2.0", "method": "blockchain.headers.subscribe", "params": [${expectedBestBlockHashHeight.asJson}]}""" + "\n" +
        s"""{"jsonrpc": "2.0", "method": "blockchain.scripthash.subscribe", "params": ["$sh", "$expectedHistoryHash"]}""" + "\n"

    output2 shouldBe expected

    (protocol2.blockchainScripthashGetHistory(sh).map(_.tx_hash) should contain).theSameElementsAs(Seq(dummyTx.txId))
  }
}
