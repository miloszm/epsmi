package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

// #confirmed tx gets reorged into another block with a different height

class ReorgDifferentBlockTest extends FlatSpec with AddressHistoryAssertions {
  "reorged to different block tx" should "have its height in history changed from the previous height to the new one" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
    val (dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx()

    val rpc = DummyBtcRpc(Seq(dummyTx1), Nil, Map(
      dummyTx1.blockhash -> containingBlockHeight1,
      dummyTx2.blockhash -> containingBlockHeight2
    ))

    val monitor = TransactionMonitorFactory.create(rpc)
    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1

    val sh = script2ScriptHash(dummySpk1)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = false)

    // #tx gets reorged into another block (so still confirmed)

    val dummyTx1ChangedBlockhash = dummyTx1.copy(blockhash = dummyTx2.blockhash)
    val rpc2 = rpc.copy(txList = Seq(dummyTx1ChangedBlockhash))
    val monitor2 = TransactionMonitorFactory.create(rpc2)
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    updatedTxs.size shouldBe 0
    monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk1, height = containingBlockHeight2, txId = dummyTx1.txId, subscribed = false)
  }
}
