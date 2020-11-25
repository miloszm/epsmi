package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummy.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ### transaction spending FROM one of our addresses

class TxSpendingFromAddressTest extends FlatSpec {
  val(dummySpk1, containingBlockHeight1, inputTx) = createDummyFundingTx()
  val(dummySpk2, containingBlockHeight2, spendingTx) = createDummyFundingTx(inputTxid = inputTx.txId)

  val rpc = DummyBtcRpc(Seq(inputTx, spendingTx), Nil, Map(
    inputTx.blockhash -> containingBlockHeight1,
    spendingTx.blockhash -> containingBlockHeight2
  ))

  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  val sh = script2ScriptHash(dummySpk1)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 2
}