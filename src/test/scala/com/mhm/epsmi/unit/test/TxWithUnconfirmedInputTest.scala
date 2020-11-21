package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

// ### unconfirmed tx arrives with unconfirmed input, which then both confirm

class TxWithUnconfirmedInputTest extends FlatSpec with AddressHistoryAssertions {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx(confirmations = 0)
  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(confirmations = 0, inputTxid = dummyTx1.txId, inputConfirmations = 0)

  val rpc = DummyBtcRpc(Nil, Seq(dummyTx1.vin, dummyTx2.vin), Map(
    dummyTx1.blockhash -> containingBlockHeight1,
    dummyTx2.blockhash -> containingBlockHeight2
  ))

  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 2

  val sh1 = script2ScriptHash(dummySpk1)
  val sh2 = script2ScriptHash(dummySpk2)
  monitorState.getElectrumHistory(sh1).getOrElse(fail).size shouldBe 0
  monitorState.getElectrumHistory(sh2).getOrElse(fail).size shouldBe 0
  val monitorState2 = monitorState.subscribeAddress(sh1).subscribeAddress(sh2)

//  # the unconfirmed transactions appear
  val (updatedTxs, monitorState3) = monitor.checkForUpdatedTxs(monitorState2)
  updatedTxs.size shouldBe 0
  val rpc2 = rpc.copy(txList = Seq(dummyTx1, dummyTx2))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs2, monitorState4) = monitor2.checkForUpdatedTxs(monitorState3)
  updatedTxs2.size shouldBe 2
  assertAddressHistoryTx(monitorState4.addressHistory, spk=dummySpk1, height = 0, txId = dummyTx1.txId, subscribed = true)
  assertAddressHistoryTx(monitorState4.addressHistory, spk=dummySpk2, height = -1, txId = dummyTx2.txId, subscribed = true)

//  # the transactions confirm

  val dummyTx1Confirmed = dummyTx1.copy(confirmations = 1)
  val dummyTx2Confirmed = dummyTx2.copy(confirmations = 1)
  val rpc3 = rpc.copy(txList = Seq(dummyTx1Confirmed, dummyTx2Confirmed))
  val monitor3 = new TransactionMonitor(rpc3, nonWalletAllowed = false)
  val (updatedTxs3, monitorState5) = monitor3.checkForUpdatedTxs(monitorState4)
  updatedTxs3.size shouldBe 2
  assertAddressHistoryTx(monitorState5.addressHistory, spk=dummySpk1, height = containingBlockHeight1, txId = dummyTx1.txId, subscribed = true)
  assertAddressHistoryTx(monitorState5.addressHistory, spk=dummySpk2, height = containingBlockHeight2, txId = dummyTx2.txId, subscribed = true)
}
