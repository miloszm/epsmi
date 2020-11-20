package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps
import org.scalatest.FlatSpec


//  ###build empty address history, subscribe one address
//  ### an unconfirmed tx appears, then confirms

class TxArrivalThenConfirmationTest extends FlatSpec with AddressHistoryAssertions {
  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)

  val rpc = DummyBtcRpc(Nil, Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1

  val sh = HashOps.script2ScriptHash(dummySpk)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

  val monitorState2 = monitorState.subscribeAddress(sh)

  val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
  updatedTxs.isEmpty shouldBe true

  // unconfirmed transaction appears
  val rpc2 = rpc.copy(txList = Seq(dummyTx))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs2, monitorState4) = monitor2.checkForUpdatedTxs(monitorState3)
  updatedTxs2.size shouldBe 1
  monitorState4.unconfirmedTxes.size shouldBe 1
  monitorState4.unconfirmedTxes.keys.head shouldBe dummyTx.txId
  assertAddressHistoryTx(monitorState4.addressHistory, spk = dummySpk, height = 0, txId = dummyTx.txId, subscribed = true)

  // transaction confirms
  val dummyTxConfirmed = dummyTx.copy(confirmations = 1)
  val rpc3 = rpc2.copy(txList = Seq(dummyTxConfirmed))
  val monitor3 = new TransactionMonitor(rpc3, nonWalletAllowed = false)
  val (updatedTxs3, monitorState5) = monitor3.checkForUpdatedTxs(monitorState4)
  assertAddressHistoryTx(monitorState5.addressHistory, spk = dummySpk, height = containingBlockHeight, txId = dummyTx.txId, subscribed = true)

}
