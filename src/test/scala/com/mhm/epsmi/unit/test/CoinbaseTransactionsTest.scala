package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec

class CoinbaseTransactionsTest extends FlatSpec with AddressHistoryAssertions {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx(coinbase = true, confirmations = 1)
  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(coinbase = true, confirmations = 101)
  val(dummySpk3, containingBlockHeight3, dummyTx3) = createDummyFundingTx(coinbase = true, confirmations = 0)
  val(dummySpk4, containingBlockHeight4, dummyTx4) = createDummyFundingTx(coinbase = true, confirmations = 1)
  val(dummySpk5, containingBlockHeight5, dummyTx5) = createDummyFundingTx(coinbase = true, confirmations = 101)
  val(dummySpk6, containingBlockHeight6, dummyTx6) = createDummyFundingTx(coinbase = true, confirmations = 0)

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
  monitorState.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bb8")

  assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = false)
  assertAddressHistoryTx(monitorState.addressHistory, spk = dummySpk2, height = containingBlockHeight2, txId = dummyTx2.txId, subscribed = false)
  val sh3 = HashOps.script2ScriptHash(dummySpk3)
  monitorState.getElectrumHistory(sh3).getOrElse(fail).size shouldBe 0

  val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx4, dummyTx5, dummyTx6))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)

  val (updatedTxs2, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
  monitorState2.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bb8", "0bbb", "0bbc")
  updatedTxs2.isEmpty shouldBe true
  assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk4, height = containingBlockHeight4, txId = dummyTx4.txId, subscribed = false)
  assertAddressHistoryTx(monitorState2.addressHistory, spk = dummySpk5, height = containingBlockHeight5, txId = dummyTx5.txId, subscribed = false)
  val sh6 = HashOps.script2ScriptHash(dummySpk6)
  monitorState2.getElectrumHistory(sh6).getOrElse(fail).size shouldBe 0

  //  test orphan tx is removed from history
  println("="*80)
  val orphanTx = dummyTx1.copy(confirmations = 0, category = "orphan")
  val rpc3 = rpc2.copy(txList = Seq(orphanTx) ++ rpc2.txList.drop(1))
  val monitor3 = new TransactionMonitor(rpc3, nonWalletAllowed = false)
  val (updatedTxs3, monitorState3) = monitor3.checkForUpdatedTxs(monitorState2)
  monitorState3.reorganizableTxes.map(_.txid.substring(0,4)) should contain theSameElementsAs Seq("0bbb")
  updatedTxs3.isEmpty shouldBe true
  val sh1 = HashOps.script2ScriptHash(dummySpk1)
  monitorState3.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 0
}
