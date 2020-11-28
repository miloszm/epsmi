package com.mhm.epsmi.roundtrip.test

import java.io.ByteArrayOutputStream

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.{TransactionMonitorFactory, TransactionMonitorState}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet, DummyTxCreator}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class HeartbeatRoundtripSpec extends FlatSpec {

  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)

  val rpc = DummyBtcRpc(Nil, Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
  val monitor = TransactionMonitorFactory.create(rpc)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1

  val sh = HashOps.script2ScriptHash(dummySpk)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

  val monitorState2 = monitorState.subscribeAddress(sh)

  val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
  updatedTxs.isEmpty shouldBe true

  // unconfirmed transaction appears
  val rpc2 = rpc.copy(txList = Seq(dummyTx))
  val monitor2 = TransactionMonitorFactory.create(rpc2)

  val protocol = new Api4ElectrumImpl(Api4ElectrumCore(DummyBtcRpc(Nil, Nil, Map())), monitor2, monitorState3)

  val streamOutput = new ByteArrayOutputStream()

  protocol.triggerHeartbeatConnected(streamOutput)

  val output = streamOutput.toString
  output.contains(""""method": "blockchain.scripthash.subscribe"""") shouldBe true
  println(output)
}
