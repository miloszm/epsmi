package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import org.scalatest.FlatSpec
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps
import org.scalatest.Matchers.convertToAnyShouldWrapper

// ###two txes with the same txid, built history

class DuplicateTxidTest extends FlatSpec {

  val(dummySpk, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val(_, containingBlockHeight2, dummyTx2Temp) = createDummyFundingTx(outputSpkOpt=Some(dummySpk))
  val(_, containingBlockHeight3, dummyTx3Temp) = createDummyFundingTx(outputSpkOpt=Some(dummySpk))

  val dummyTx2 = dummyTx2Temp.copy(txId = dummyTx1.txId)
  val dummyTx3 = dummyTx3Temp.copy(txId = dummyTx1.txId)

  val sh = HashOps.script2ScriptHash(dummySpk)

  val rpc = DummyBtcRpc(Seq(dummyTx1, dummyTx2), Nil, Map(
    dummyTx1.blockhash -> containingBlockHeight1,
    dummyTx2.blockhash -> containingBlockHeight2,
    dummyTx3.blockhash -> containingBlockHeight3
  ))

  val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  monitorState.getElectrumHistory(sh).getOrElse(fail).map(_.txHash).head shouldBe dummyTx1.txId
  val monitorState2 = monitorState.subscribeAddress(sh)
  monitorState2.getElectrumHistory(sh).getOrElse(fail).map(_.txHash).head shouldBe dummyTx1.txId
  val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx3))
  val monitor2 = new TransactionMonitor(rpc2, nonWalletAllowed = false)
  val (updatedTxs, monitorState3) = monitor2.checkForUpdatedTxs(monitorState2)
  updatedTxs.size shouldBe 1
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
  monitorState.getElectrumHistory(sh).getOrElse(fail).map(_.txHash).head shouldBe dummyTx1.txId

}
