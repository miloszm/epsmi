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

    val dummyBtcRpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx("blockhash").asInstanceOf[String] -> BlockHeight))

    val transactionMonitor = new TransactionMonitor(dummyBtcRpc, rawMode = false)

    val tx = Tx4HistoryGen(
      dummyTx("confirmations").asInstanceOf[Int],
      dummyTx("txid").asInstanceOf[String],
      DoubleSha256DigestBE.fromHex(dummyTx("blockhash").asInstanceOf[String])
    )
    val txd = dummyBtcRpc.toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe BlockHeight
    newHistoryElement.txHash shouldBe dummyTx("txid").asInstanceOf[String]
  }

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is zero" in {
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx(confirmations = 0)

    val utxoSet = Seq(dummyTx("vin").asInstanceOf[Map[String, Any]])
    val dummyBtcRpc = new DummyBtcRpc(Seq(dummyTx), utxoSet, Map(dummyTx("blockhash").asInstanceOf[String] -> 133))

    val transactionMonitor = new TransactionMonitor(dummyBtcRpc, rawMode = false)

    val tx = Tx4HistoryGen(
      dummyTx("confirmations").asInstanceOf[Int],
      dummyTx("txid").asInstanceOf[String],
      DoubleSha256DigestBE.fromHex(dummyTx("blockhash").asInstanceOf[String])
    )
    val txd = dummyBtcRpc.toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 0
    newHistoryElement.txHash shouldBe dummyTx("txid").asInstanceOf[String]
    newHistoryElement.fee shouldBe BigDecimal(2)
  }

  "transaction monitor" should "have functionality for getInputAndOutputScriptpubkeys" in {
    val InputTxid = "a0"*32
    val ThisTxid = "b0"*32
    val ThisOutSpk = "abab"
    val ThisInSpk = "cdcd"

    val(_, _, inputTx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(ThisInSpk), hexDifferentiator = 3)
    assert(inputTx("txid") == InputTxid)
    assert(inputTx("vout").asInstanceOf[Map[String,Any]]("scriptPubKey").asInstanceOf[String] == ThisInSpk)

    val(_, _, tx) = DummyTxCreator.createDummyFundingTx(outputSpkOpt = Some(ThisOutSpk), inputTxid = InputTxid, txId = ThisTxid, hexDifferentiator = 4)
    assert(tx("txid") == ThisTxid)
    assert(tx("vout").asInstanceOf[Map[String,Any]]("scriptPubKey").asInstanceOf[String] == ThisOutSpk)
    assert(tx("vin").asInstanceOf[Map[String,Any]]("txid").asInstanceOf[String] == InputTxid)

    val dummyBtcRpc = new DummyBtcRpc(Seq(tx, inputTx))

    val(outputScriptpubkeys, inputScriptpubkeys, tr) = new TransactionMonitor(dummyBtcRpc, rawMode = false).getInputAndOutputScriptpubkeys(
      DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String])
    )
    outputScriptpubkeys should contain theSameElementsAs Seq(ThisOutSpk)
    inputScriptpubkeys should contain theSameElementsAs Seq(ThisInSpk)
    tr.txid.hex shouldBe ThisTxid
  }
}
