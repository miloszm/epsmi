package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import org.scalatest.FlatSpec

//  ### one unconfirmed tx in wallet belonging to us, with confirmed inputs,
//  ### addr history built, then tx confirms, not subscribed to address

class NonSubscribedConfirmationTest extends FlatSpec with AddressHistoryAssertions {

  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)

  val rpc = DummyBtcRpc(Seq(dummyTx), Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1

  assertAddressHistoryTx(monitorState.addressHistory, spk=dummySpk, height=0, txId = dummyTx.txId, subscribed = false)

  val (updatedTxs, monitorState2) = monitor.checkForUpdatedTxs(monitorState)
  monitorState2.unconfirmedTxes.size shouldBe 1
  updatedTxs.size shouldBe 0

  val dummyTxConfirmed = dummyTx.copy(confirmations = 1) // tx confirms
  val rpc2 = rpc.copy(txList = Seq(dummyTxConfirmed))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs2, monitorState3) = monitor2.checkForUpdatedTxs(monitorState2)
  // #not subscribed so still only returns an empty list
  updatedTxs2.size shouldBe 0
  monitorState3.reorganizableTxes.size shouldBe 1
  assertAddressHistoryTx(monitorState3.addressHistory, spk=dummySpk, height=containingBlockHeight, txId = dummyTx.txId, subscribed = false)
}
