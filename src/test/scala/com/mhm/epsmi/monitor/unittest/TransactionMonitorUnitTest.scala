package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.{TransactionMonitorImpl, Tx4HistoryGen}
import com.mhm.epsmi.dummymonitor.DummyBtcRpc.toRpcTransaction
import com.mhm.epsmi.dummymonitor.{DummyBtcRpc, DummyTxCreator}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class TransactionMonitorUnitTest extends FlatSpec {

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is not zero" in {
    val BlockHeight = 133
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx()

    val dummyBtcRpc = DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> BlockHeight))

    val transactionMonitor = new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false)

    val tx = Tx4HistoryGen(
      dummyTx.confirmations,
      dummyTx.txId,
      Some(DoubleSha256DigestBE.fromHex(dummyTx.blockhash))
    )
    val txd = toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe BlockHeight
    newHistoryElement.txHash shouldBe dummyTx.txId
  }

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is zero" in {
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx(confirmations = 0)

    val utxoSet = Seq(dummyTx.vin)
    val dummyBtcRpc = DummyBtcRpc(Seq(dummyTx), utxoSet, Map(dummyTx.blockhash -> 133))

    val transactionMonitor = new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false)

    val tx = Tx4HistoryGen(
      dummyTx.confirmations,
      dummyTx.txId,
      Some(DoubleSha256DigestBE.fromHex(dummyTx.blockhash))
    )
    val txd = toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 0
    newHistoryElement.txHash shouldBe dummyTx.txId
    newHistoryElement.fee shouldBe BigDecimal(2)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val OutSpk = "abab"
    val InSpk = "cdcd"

    val(_, _, inputTx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(InSpk))
    assert(inputTx.vout.scriptPubKey == InSpk)

    val(_, _, tx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(OutSpk), inputTxid = inputTx.txId)
    assert(tx.vout.scriptPubKey == OutSpk)
    assert(tx.vin.txId == inputTx.txId)

    val dummyBtcRpc = DummyBtcRpc(Seq(tx, inputTx))

    val(outputScriptpubkeys, inputScriptpubkeys, tr) = new TransactionMonitorImpl(dummyBtcRpc, nonWalletAllowed = false).getInputAndOutputScriptpubkeys(
      DoubleSha256DigestBE.fromHex(tx.txId)
    )
    outputScriptpubkeys should contain theSameElementsAs Seq(OutSpk)
    inputScriptpubkeys should contain theSameElementsAs Seq(InSpk)
    tr.txid.hex shouldBe tx.txId
  }
}
