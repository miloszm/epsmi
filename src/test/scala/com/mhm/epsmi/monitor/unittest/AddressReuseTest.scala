package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorFactory, TransactionMonitorImpl}
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class AddressReuseTest extends FlatSpec {
  val(dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
  val(dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(outputSpkOpt=Some(dummySpk1))

  val rpc = DummyBtcRpc(Seq(dummyTx1), Nil, Map(
    dummyTx1.blockhash -> containingBlockHeight1,
    dummyTx2.blockhash -> containingBlockHeight2
  ))

  val monitor = TransactionMonitorFactory.create(rpc)

  val monitorState = monitor.buildAddressHistory(Seq(dummySpk1), Seq(new DummyDeterministicWallet))
  val sh = script2ScriptHash(dummySpk1)
  monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1

  "transaction arriving to an address with nonempty transaction history" should "be added to this address' history" in {
    val rpc2 = rpc.copy(txList = rpc.txList ++ Seq(dummyTx2))
    val monitor2 = TransactionMonitorFactory.create(rpc2)
    monitorState.getElectrumHistory(sh).getOrElse(fail).size shouldBe 1
    val (updatedTxs, monitorState2) = monitor2.checkForUpdatedTxs(monitorState)
    monitorState2.getElectrumHistory(sh).getOrElse(fail).size shouldBe 2
  }
}
