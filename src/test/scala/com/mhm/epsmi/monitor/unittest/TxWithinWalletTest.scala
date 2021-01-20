package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.dummymonitor.DummyTxCreator.createDummyFundingTx
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyDeterministicWallet}
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class TxWithinWalletTest extends FlatSpec {

  "transaction from one address to the other, both addresses in wallet" should "be present in history for 2 addresses" in {
    val (dummySpk1, containingBlockHeight1, dummyTx1) = createDummyFundingTx()
    val (dummySpk2, containingBlockHeight2, dummyTx2) = createDummyFundingTx(inputTxid = dummyTx1.txId)

    val rpc = DummyBtcRpc(
      Seq(dummyTx1, dummyTx2),
      Nil,
      Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2)
    )

    val monitor = TransactionMonitorFactory.create(rpc)

    val monitorState = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))

    val dummySpk1History = monitorState.getElectrumHistory(script2ScriptHash(dummySpk1)).getOrElse(fail)
    val dummySpk2History = monitorState.getElectrumHistory(script2ScriptHash(dummySpk2)).getOrElse(fail)
    dummySpk1History.size shouldBe 2
    dummySpk2History.size shouldBe 1
    dummySpk1History.map(_.txHash) should contain(dummySpk2History.map(_.txHash).head)

    /**
    * dummyTx1 is in block 1000 and its history is that it is a VIN in dummyTx2 in block 1001
    * dummyTx2 is in block 1001 and does not have any subsequent history (within wallet)
    */
  }
}
