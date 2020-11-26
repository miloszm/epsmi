package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec

//  ###transaction that has nothing to do with our wallet

class UnrelatedTxTest extends FlatSpec with AddressHistoryAssertions {

  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = 0)
  val ourDummySpk = "beef"*16

  val rpc = DummyBtcRpc(Seq(dummyTx), Seq(dummyTx.vin), Map(dummyTx.blockhash -> containingBlockHeight))
  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(ourDummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1
  monitorState.getElectrumHistory(script2ScriptHash(ourDummySpk)) shouldBe Some(Nil)

}
