package com.mhm.epsmi.dummy

import java.util.concurrent.atomic.AtomicInteger

import org.bitcoins.core.protocol.transaction.BaseTransaction

object DummyTxCreator {

  val masterDummyId = new AtomicInteger(1000)

  def dummySpkToAddress(spk: String): String = "1FpeH5RojTMpaUS8oreYBRtMpCk1mfVxcf"

  def createDummyFundingTx(
    confirmations: Int = 1,
    outputSpkOpt: Option[String] = None,
    inputTxid: String = "e0"*32,
    txId: String = "a0"*32,
    coinbase: Boolean = false,
    inputConfirmations: Int = 1,
    hexDifferentiator: Int = 15900): (String, Int, Map[String, Any]) = {
    val dummyId = masterDummyId.getAndIncrement()
    val dummySpk = outputSpkOpt match {
      case Some(outputSpk) => outputSpk
      case None => "a9"*32
    }
    val dummyContainingBlock = "bb"*32
    val containingBlockHeight = dummyId
    val category = if (!coinbase) "receive" else {
      if (confirmations < 1) "orphan"
      else if (confirmations <= 100) "immature"
      else "generate"
    }
    val vin = scala.collection.mutable.Map (
      "txid" -> inputTxid,
      "vout" -> 0,
      "value" -> 100,
      "confirmations" -> inputConfirmations,
    )
    if (coinbase)
      vin.put("coinbase", "nonce")
    val vout = Map (
      "value" -> 98,
      "scriptPubKey" -> dummySpk
    )

    val dummyTx = Map(
      "txid" -> txId,
      "vin" -> vin.toMap,
      "vout" -> vout,
      "address" -> dummySpkToAddress(dummySpk),
      "category" -> category,
      "confirmations" -> confirmations,
      "blockhash" -> dummyContainingBlock,
      "hex" -> BaseTransaction(org.bitcoins.core.number.Int32(0), Nil, Nil, org.bitcoins.core.number.UInt32(hexDifferentiator))
    )

    (dummySpk, containingBlockHeight, dummyTx)
  }

}
