package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

// ### an unconfirmed tx being broadcast, another conflicting tx being
// ### confirmed, the first tx gets conflicted status

class ReorgFinneyAttackTest extends FlatSpec with AddressHistoryAssertions {
  "tx1 unconfirmed becomes conflicted, tx2 that has tx1 as input, confirms (finney attack)" should "remove tx1 from history, add tx2 to history" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx(confirmations = 0)
    val (dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(confirmations = 0, inputTxid = dummyTx1.vin.txId)

    val rpc = DummyBtcRpc(Seq(dummyTx1), Seq(dummyTx1.vin), Map(
      dummyTx1.blockhash -> containingBlockHeight1,
      dummyTx2.blockhash -> containingBlockHeight2
    ))

    val monitor = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))
    monitorState.lastKnownTx.map(_.txid) shouldBe Some(dummyTx1.txId)
    monitorState.addressHistory.m.size shouldBe 2

    val sh1 = script2ScriptHash(dummySpk1)
    val sh2 = script2ScriptHash(dummySpk2)
    monitorState.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 1
    monitorState.getElectrumHistory(sh2).getOrElse(fail).size shouldBe 0
    assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk1, height = 0, txId = dummyTx1.txId, subscribed = false)

    // # a conflicting transaction confirms
    val dummyTx1Conflicted = dummyTx1.copy(confirmations = -1)
    val dummyTx2Confirmed = dummyTx2.copy(confirmations = 1)
    val rpc2 = rpc.copy(txList = Seq(dummyTx1Conflicted, dummyTx2Confirmed))
    val monitor2 = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    updatedTxs.size shouldBe 0
    monitorState2.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 0
    monitorState2.getElectrumHistory(sh2).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk2, height = containingBlockHeight2, txId = dummyTx2.txId, subscribed = false)
  }
}
