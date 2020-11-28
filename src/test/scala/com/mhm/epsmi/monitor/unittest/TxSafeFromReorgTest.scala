package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorFactory}
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ## tx confirmed with 1 confirmation, then confirmations goes to 100
// ## test that the reorganizable_txes list length goes down

class TxSafeFromReorgTest extends FlatSpec {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val rpc = DummyBtcRpc(Seq(dummyTx1), Nil, Map(dummyTx1.blockhash -> containingBlockHeight1))
  val monitor = TransactionMonitorFactory.create(rpc)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  val (updatedTxs, monitorState2) = monitor.checkForUpdatedTxs(monitorState)
  updatedTxs.size shouldBe 0
  monitorState2.reorganizableTxes.size shouldBe 1
  val dummyTx1Unreorganizable = dummyTx1.copy(confirmations = 2000)
  val rpc2 = rpc.copy(txList = Seq(dummyTx1Unreorganizable))
  val monitor2 = TransactionMonitorFactory.create(rpc2)
  val (updatedTxs2, monitorState3) = monitor2.checkForUpdatedTxs(monitorState2)
  updatedTxs2.size shouldBe 0
  monitorState3.reorganizableTxes.size shouldBe 0
}
