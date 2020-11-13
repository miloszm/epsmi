package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.{AddressHistory, TransactionMonitor}
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet, DummyTxCreator}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class BuildAddressHistoryTest extends FlatSpec {

  def assertAddressHistoryTx(addressHistory: AddressHistory, spk: String, height: Int, txId: String, subscribed: Boolean): Unit = {
    val historyElement = addressHistory.m.getOrElse(HashOps.script2ScriptHash(spk), fail)
    historyElement.history.head.height shouldBe height
    historyElement.history.head.txHash shouldBe txId
    if (height == 0)
      historyElement.history.head.fee shouldBe 0
    historyElement.subscribed shouldBe subscribed
  }

  "transaction monitor" should "build single entry address history" in {
    val (dummySpk, blockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx()
    val rpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> blockHeight))
    val monitor = new TransactionMonitor(rpc, false)
    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    addressHistory.m.size shouldBe 1
    assertAddressHistoryTx(addressHistory, dummySpk, blockHeight, dummyTx.txId, subscribed = false)
  }

  "transaction monitor" should " build address history with two entries" in {
    val(dummySpk1, containingBlockHeight1, dummyTx1) = DummyTxCreator.createDummyFundingTx()
    val(dummySpk2, containingBlockHeight2, dummyTx2) = DummyTxCreator.createDummyFundingTx()
    val rpc = new DummyBtcRpc(Seq(dummyTx1, dummyTx2), Nil, Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2))
    val monitor = new TransactionMonitor(rpc, false)
    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))
    addressHistory.m.size shouldBe 2
    assertAddressHistoryTx(addressHistory, dummySpk1, containingBlockHeight1, dummyTx1.txId, subscribed = false)
    assertAddressHistoryTx(addressHistory, dummySpk2, containingBlockHeight2, dummyTx2.txId, subscribed = false)
  }

  "transaction monitor" should " build address history with many entries" in {
    val(inputSpk, inputBlockHeight1, inputTx) = DummyTxCreator.createDummyFundingTx()
    val(dummySpk, containingBlockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx(
        confirmations = 0, inputTxid = inputTx.vin.txId
    )
    val sh = HashOps.script2ScriptHash(dummySpk)
    val InitialTxCount = 1100 // we want to exceed the batch size of 1000
    val txs = for (_ <- 0 until InitialTxCount) yield {
      val (_, _, tx) = DummyTxCreator.createDummyFundingTx(
        outputSpkOpt = Some(dummySpk),
        inputTxid = inputTx.vin.txId,
        confirmations = 0
      )
      tx
    }
    txs.length shouldBe InitialTxCount

    val rpc = new DummyBtcRpc(txs, Seq(dummyTx.vin))
    val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
    addressHistory.m.size shouldBe 1
    // todo assert len(list(txmonitor.check_for_updated_txes())) == 0
    addressHistory.m(sh).history.length shouldBe InitialTxCount

    val AddedTxCount = 130
    val newTxs = for (_ <- 0 until AddedTxCount) yield {
      val (_, _, tx) = DummyTxCreator.createDummyFundingTx(
        outputSpkOpt = Some(dummySpk),
        inputTxid = inputTx.vin.txId,
        confirmations = 0
      )
      tx
    }

    val newRpc = rpc.copy(txList = rpc.txList ++ newTxs)
    // TODO finish this after
    //    check_for_new_txes
    //    check_for_confirmations
    //    check_for_reorganizations
    //    are implemented in transaction monitor
    //    test_transactionmonitor.py line 257
  }

}
