package com.mhm.epsmi.modules

import com.mhm.bitcoin.{TransactionMonitor, Tx4HistoryGen}
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyTxCreator}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class TransactionMonitorDummyTest extends FlatSpec {

  "transaction monitor" should "have functionality for generating new history element when number of confirmations is not zero" in {
    val(_, _, dummyTx) = DummyTxCreator.createDummyFundingTx()

    val dummyBtcRpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx("blockhash").asInstanceOf[String] -> 133))

    val transactionMonitor = new TransactionMonitor(dummyBtcRpc, rawMode = false)

    val tx = Tx4HistoryGen(
      dummyTx("confirmations").asInstanceOf[Int],
      dummyTx("txid").asInstanceOf[String],
      DoubleSha256DigestBE.fromHex(dummyTx("blockhash").asInstanceOf[String])
    )
    val txd = dummyBtcRpc.toRpcTransaction(dummyTx)

    val newHistoryElement = transactionMonitor.generateNewHistoryElement(tx, txd)
    newHistoryElement.height shouldBe 133
    newHistoryElement.txHash shouldBe "a0"*32
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
    newHistoryElement.txHash shouldBe "a0"*32
    newHistoryElement.fee shouldBe BigDecimal(2)
  }

}
