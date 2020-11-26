package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

class CoinbaseTransactionsTest extends FlatSpec with AddressHistoryAssertions {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx(masterId = 1000, coinbase = true, confirmations = 1)   // bb8
  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(masterId = 1001, coinbase = true, confirmations = 101) // bb9
  val(dummySpk3, containingBlockHeight3, dummyTx3) = createDummyFundingTx(masterId = 1002, coinbase = true, confirmations = 0)   // bba
  val(dummySpk4, containingBlockHeight4, dummyTx4) = createDummyFundingTx(masterId = 1003, coinbase = true, confirmations = 1)   // bbb
  val(dummySpk5, containingBlockHeight5, dummyTx5) = createDummyFundingTx(masterId = 1004, coinbase = true, confirmations = 101) // bbc
  val(dummySpk6, containingBlockHeight6, dummyTx6) = createDummyFundingTx(masterId = 1005, coinbase = true, confirmations = 0)   // bbd

  val rpc = DummyBtcRpc(Seq(dummyTx1, dummyTx2, dummyTx3), Nil,
    Map(
      dummyTx1.blockhash -> containingBlockHeight1,
      dummyTx2.blockhash -> containingBlockHeight2,
      dummyTx3.blockhash -> containingBlockHeight3,
      dummyTx4.blockhash -> containingBlockHeight4,
      dummyTx5.blockhash -> containingBlockHeight5,
      dummyTx6.blockhash -> containingBlockHeight6
    )
  )

  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2, dummySpk3, dummySpk4, dummySpk5, dummySpk6), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 6

  assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = false)
  assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk2, height = containingBlockHeight2, txId = dummyTx2.txId, subscribed = false)
  val sh3 = script2ScriptHash(dummySpk3)
  monitorState.getElectrumHistory(sh3).getOrElse(fail).size shouldBe 0

  val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx4, dummyTx5, dummyTx6))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)

  monitorState.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bb8")
  val (updatedTxs2, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
  monitorState2.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bb8", "0bbb")
  updatedTxs2.isEmpty shouldBe true
  assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk4, height = containingBlockHeight4, txId = dummyTx4.txId, subscribed = false)
  assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk5, height = containingBlockHeight5, txId = dummyTx5.txId, subscribed = false)
  val sh6 = script2ScriptHash(dummySpk6)
  monitorState2.getElectrumHistory(sh6).getOrElse(fail).size shouldBe 0

  //  test orphan tx is removed from history
  val orphanTx = dummyTx1.copy(confirmations = 0, category = "orphan")
  val rpc3 = rpc2.copy(txList = Seq(orphanTx) ++ rpc2.txList.drop(1))
  val monitor3 = new TransactionMonitor(rpc3, nonWalletAllowed = false)
  monitorState2.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bb8", "0bbb")
  val (updatedTxs3, monitorState3) = monitor3.checkForUpdatedTxs(monitorState2)
  monitorState3.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bbb")
  updatedTxs3.isEmpty shouldBe true
  val sh1 = script2ScriptHash(dummySpk1)
  monitorState3.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 0
}
