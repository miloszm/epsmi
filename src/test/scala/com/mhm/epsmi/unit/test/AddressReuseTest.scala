package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ### transaction which arrives to an address which already has a tx on it

class AddressReuseTest extends FlatSpec {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(outputSpkOpt=Some(dummySpk1))

  val rpc = DummyBtcRpc(Seq(dummyTx1), Nil, Map(
    dummyTx1.blockhash -> containingBlockHeight1,
    dummyTx2.blockhash -> containingBlockHeight2
  ))

  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  val sh = script2ScriptHash(dummySpk1)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1

  val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx2))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
  monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 2
}
