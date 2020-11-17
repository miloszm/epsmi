package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.{AddressHistory, LastKnown, TransactionMonitor, TxidAddress}
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyDeterministicWallet, DummyTxCreator}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class BuildAddressHistoryTest extends FlatSpec {

//  def assertAddressHistoryTx(addressHistory: AddressHistory, spk: String, height: Int, txId: String, subscribed: Boolean): Unit = {
//    val historyElement = addressHistory.m.getOrElse(HashOps.script2ScriptHash(spk), fail)
//    historyElement.history.head.height shouldBe height
//    historyElement.history.head.txHash shouldBe txId
//    if (height == 0)
//      historyElement.history.head.fee shouldBe 0
//    historyElement.subscribed shouldBe subscribed
//  }

//  "transaction monitor" should "build single entry address history" in {
//    val (dummySpk, blockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx()
//    val rpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> blockHeight))
//    val monitor = new TransactionMonitor(rpc, false)
//    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
//    addressHistory.m.size shouldBe 1
//    assertAddressHistoryTx(addressHistory, dummySpk, blockHeight, dummyTx.txId, subscribed = false)
//  }

//  "transaction monitor" should " build address history with two entries" in {
//    val(dummySpk1, containingBlockHeight1, dummyTx1) = DummyTxCreator.createDummyFundingTx()
//    val(dummySpk2, containingBlockHeight2, dummyTx2) = DummyTxCreator.createDummyFundingTx()
//    val rpc = new DummyBtcRpc(Seq(dummyTx1, dummyTx2), Nil, Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2))
//    val monitor = new TransactionMonitor(rpc, false)
//    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2), Seq(new DummyDeterministicWallet))
//    addressHistory.m.size shouldBe 2
//    assertAddressHistoryTx(addressHistory, dummySpk1, containingBlockHeight1, dummyTx1.txId, subscribed = false)
//    assertAddressHistoryTx(addressHistory, dummySpk2, containingBlockHeight2, dummyTx2.txId, subscribed = false)
//  }

//  "transaction monitor" should " build address history with many entries" in {
//    val(inputSpk, inputBlockHeight1, inputTx) = DummyTxCreator.createDummyFundingTx()
//    val(dummySpk, containingBlockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx(confirmations = 0, inputTxid = inputTx.vin.txId)
//    val sh = HashOps.script2ScriptHash(dummySpk)
//
//
//    val InitialTxCount = 1100 // we want to exceed the batch size of 1000
//    val txs1 = Seq(dummyTx)
//    val txs2 = for (i <- 0 until InitialTxCount-1) yield {
//      val (_, _, tx) = DummyTxCreator.createDummyFundingTx(
//        outputSpkOpt = Some(dummySpk),
//        inputTxid = inputTx.vin.txId,
//        confirmations = 0
//      )
//      tx
//    }
//    val txs = txs1 ++ txs2
//    txs.length shouldBe InitialTxCount
//
//    val rpc = DummyBtcRpc(txs, Seq(dummyTx.vin))
//    val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
//    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk), Seq(new DummyDeterministicWallet))
//    monitor.lastKnown.get shouldBe LastKnown(Some(TxidAddress(txs(monitor.BATCH_SIZE).txId, txs.head.address)))
//    addressHistory.m.size shouldBe 1
//    monitor.checkForUpdatedTxs(addressHistory).size shouldBe 0
//    monitor.lastKnown.get shouldBe LastKnown(Some(TxidAddress(txs(monitor.BATCH_SIZE).txId, txs.head.address)))
//    addressHistory.m(sh).history.length shouldBe InitialTxCount
//
//    val AddedTxCount = 130
//    val newTxs = for (i <- 0 until AddedTxCount) yield {
//      val (_, _, tx) = DummyTxCreator.createDummyFundingTx(
//        outputSpkOpt = Some(dummySpk),
//        inputTxid = inputTx.vin.txId,
//        confirmations = 0
//      )
//      println(s"$i ${tx.txId}")
//      tx
//    }
//
//    val newRpc = rpc.copy(txList = rpc.txList ++ newTxs)
//    val newMonitor = new TransactionMonitor(newRpc, nonWalletAllowed = false, initLastKnown = monitor.lastKnown.get)
//    newMonitor.checkForUpdatedTxs(addressHistory).size shouldBe 0
//    addressHistory.m(sh).history.length shouldBe InitialTxCount+AddedTxCount
//  }

  "transaction monitor" should "have checking for new transactions functionality" in {
    val(dummySpk1, containingBlockHeight1, dummyTx1) = DummyTxCreator.createDummyFundingTx()
    val(dummySpk2, containingBlockHeight2, dummyTx2) = DummyTxCreator.createDummyFundingTx()
    val(dummySpk3, containingBlockHeight3, dummyTx3) = DummyTxCreator.createDummyFundingTx()

    val rpc = DummyBtcRpc(Seq(dummyTx1, dummyTx2, dummyTx3),
                          Nil,
                          Map(dummyTx1.blockhash -> containingBlockHeight1, dummyTx2.blockhash -> containingBlockHeight2, dummyTx3.blockhash -> containingBlockHeight3))

    val monitor = new TransactionMonitor(rpc, nonWalletAllowed = false)
    val buildResult = monitor.buildAddressHistory(Seq(dummySpk1, dummySpk2, dummySpk3), Seq(new DummyDeterministicWallet))
    println(s"ah=${buildResult.addressHistory.m.toList.mkString("\n")}")
    val lastKnown1 = buildResult.lastKnown
    println(s"       lastKnown0=${buildResult.lastKnown}")

    val result1 = monitor.checkForNewTxs(buildResult.addressHistory, buildResult.lastKnown)
    println(s"       lastKnown1=${result1.lastKnown}")
    println(s"       newFound1=${result1.newFound}")
    println(s"       new ah=${result1.newHistoryElements}")
    println(s"       new unconfirmed=${result1.newUnconfirmed}")
    println(s"       new reorganizable=${result1.newReorganizable}")

//    val result2 = monitor.checkForNewTxs(ah, result1.lastKnown)
//    result2.lastKnown shouldBe result1.lastKnown
//    result2.newFound shouldBe Set()
//
//    val(dummySpk4, containingBlockHeight4, dummyTx4) = DummyTxCreator.createDummyFundingTx()


  }

}