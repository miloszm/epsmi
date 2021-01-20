package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import org.scalatest.FlatSpec

class UnrelatedTxTest extends FlatSpec with AddressHistoryAssertions {

  "transaction that has nothing to do with our wallet" should "not enter history" in {
    val (_, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)
    val ourDummySpk                         = "beef" * 16

    val rpc     = DummyBtcRpc(Seq(dummyTx), Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
    val monitor = TransactionMonitorFactory.create(rpc)

    val monitorState = monitor.buildAddressHistory(Seq(ourDummySpk), Seq(new DummyDeterministicWallet))
    monitorState.addressHistory.m.size shouldBe 1
    assertHistoryEmpty(monitorState.addressHistory, ourDummySpk)
  }

}
