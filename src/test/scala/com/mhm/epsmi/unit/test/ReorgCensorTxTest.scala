package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

// # confirmed tx gets reorg'd out and becomes unconfirmed

class ReorgCensorTxTest extends FlatSpec with AddressHistoryAssertions {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val rpc = DummyBtcRpc(Seq(dummyTx1), Seq(dummyTx1.vin), Map(dummyTx1.blockhash -> containingBlockHeight1))
  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1

  val sh = script2ScriptHash(dummySpk1)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  assertAddressHistoryTx(monitorState.addressHistory, spk=dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = false)

  //  #blocks appear which reorg'd out the tx, making it unconfirmed
  val dummyTx1Unconfirmed = dummyTx1.copy(confirmations = 0)
  val rpc2 = rpc.copy(txList = Seq(dummyTx1Unconfirmed))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
  updatedTxs.size shouldBe 0
  monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  assertAddressHistoryTx(monitorState2.addressHistory, spk=dummySpk1, height = 0, txId = dummyTx1.txId, subscribed = false)
}
