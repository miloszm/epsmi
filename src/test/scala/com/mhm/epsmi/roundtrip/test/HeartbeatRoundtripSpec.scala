package com.mhm.epsmi.roundtrip.test

import java.io.ByteArrayOutputStream

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.{HistoryElement, HistoryEntry, TransactionMonitorFactory}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class HeartbeatRoundtripSpec extends FlatSpec {

  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)
  val sh = HashOps.script2ScriptHash(dummySpk)
  val rpc = DummyBtcRpc(Nil, Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
  val monitor = TransactionMonitorFactory.create(rpc)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1
  monitorState.addressHistory.m shouldBe Map(sh -> HistoryEntry(subscribed = false, List()))
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

  println(s"subscribing address: $sh")
  val monitorState2 = monitorState.subscribeAddress(sh)
  monitorState2.addressHistory.m shouldBe Map(sh -> HistoryEntry(subscribed = true, List()))

  val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
  updatedTxs.isEmpty shouldBe true
  monitorState3.updatedScripthashes shouldBe Nil

  println(s"unconfirmed transaction appears: ${dummyTx.txId}")
  val rpc2 = rpc.copy(txList = Seq(dummyTx))
  val monitor2 = TransactionMonitorFactory.create(rpc2)

  val protocol = new Api4ElectrumImpl(Api4ElectrumCore(DummyBtcRpc(Nil, Nil, Map())), monitor2, monitorState3)

  val streamOutput = new ByteArrayOutputStream()

  protocol.triggerHeartbeatConnected(streamOutput)

  protocol.currentMonitorState.get().getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  protocol.currentMonitorState.get().addressHistory.m shouldBe Map(sh -> HistoryEntry(subscribed = true,List(HistoryElement(dummyTx.txId,0,2))))
  val output = streamOutput.toString
  val expectedHistoryHash = protocol.currentMonitorState.get.getElectrumHistoryHash(sh)
  output shouldBe s"""{"method": "blockchain.scripthash.subscribe", "params": [$sh, $expectedHistoryHash]}"""
}
