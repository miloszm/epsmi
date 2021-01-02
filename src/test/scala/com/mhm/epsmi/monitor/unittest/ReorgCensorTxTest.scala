package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

// # confirmed tx gets reorg'd out and becomes unconfirmed

class ReorgCensorTxTest extends FlatSpec with AddressHistoryAssertions {
  "reorged out tx" should "have its height in history changed from non-zero to zero" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
    val rpc = DummyBtcRpc(Seq(dummyTx1), Seq(dummyTx1.vin), Map(dummyTx1.blockhash -> containingBlockHeight1))
    val monitor = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1

    val sh = script2ScriptHash(dummySpk1)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = false)

    //  #blocks appear which reorg'd out the tx, making it unconfirmed
    val dummyTx1Unconfirmed = dummyTx1.copy(confirmations = 0)
    val rpc2 = rpc.copy(txList = Seq(dummyTx1Unconfirmed))
    val monitor2 = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    updatedTxs.size shouldBe 0
    monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk1, height = 0, txId = dummyTx1.txId, subscribed = false)
  }
}
