package com.mhm.epsmi.modules

import com.mhm.bitcoin.{TransactionMonitor, Tx4HistoryGen}
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyTxCreator}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class TransactionMonitorDummyTest extends FlatSpec {

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is not zero" in {
    val BlockHeight = 133
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx()

    val dummyBtcRpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> BlockHeight))

    val transactionMonitor = new TransactionMonitor(dummyBtcRpc, rawMode = false)

    val tx = Tx4HistoryGen(
      dummyTx.confirmations,
      dummyTx.txId,
      DoubleSha256DigestBE.fromHex(dummyTx.blockhash)
    )
    val txd = dummyBtcRpc.toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe BlockHeight
    newHistoryElement.txHash shouldBe dummyTx.txId
  }

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is zero" in {
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx(confirmations = 0)

    val utxoSet = Seq(dummyTx.vin)
    val dummyBtcRpc = new DummyBtcRpc(Seq(dummyTx), utxoSet, Map(dummyTx.blockhash -> 133))

    val transactionMonitor = new TransactionMonitor(dummyBtcRpc, rawMode = false)

    val tx = Tx4HistoryGen(
      dummyTx.confirmations,
      dummyTx.txId,
      DoubleSha256DigestBE.fromHex(dummyTx.blockhash)
    )
    val txd = dummyBtcRpc.toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 0
    newHistoryElement.txHash shouldBe dummyTx.txId
    newHistoryElement.fee shouldBe BigDecimal(2)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val ThisOutSpk = "abab"
    val ThisInSpk = "cdcd"

    val(_, _, inputTx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(ThisInSpk))
    assert(inputTx.vout.scriptPubKey == ThisInSpk)

    val(_, _, tx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(ThisOutSpk), inputTxid = inputTx.txId)
    assert(tx.vout.scriptPubKey == ThisOutSpk)
    assert(tx.vin.txId == inputTx.txId)

    val dummyBtcRpc = new DummyBtcRpc(Seq(tx, inputTx))

    val(outputScriptpubkeys, inputScriptpubkeys, tr) = new TransactionMonitor(dummyBtcRpc, rawMode = false).getInputAndOutputScriptpubkeys(
      DoubleSha256DigestBE.fromHex(tx.txId)
    )
    outputScriptpubkeys should contain theSameElementsAs Seq(ThisOutSpk)
    inputScriptpubkeys should contain theSameElementsAs Seq(ThisInSpk)
    tr.txid.hex shouldBe tx.txId
  }
}
