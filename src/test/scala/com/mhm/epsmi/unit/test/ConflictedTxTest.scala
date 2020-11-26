package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ### conflicted transaction should get rejected

class ConflictedTxTest extends FlatSpec {
  val(dummySpk, containingBlockHeight, dummyTx) = createDummyFundingTx(confirmations = -1)
  val rpc = DummyBtcRpc(Seq(dummyTx), Nil, Map())
  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
  val sh = script2ScriptHash(dummySpk)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.addressHistory.m.size shouldBe 1

  // #shouldnt show up after build history because conflicted
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0

  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(confirmations = -1, outputSpkOpt = Some(dummySpk))
  val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx2))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
  updatedTxs.size shouldBe 0

//  #incoming tx is not added either
  monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 0
}
