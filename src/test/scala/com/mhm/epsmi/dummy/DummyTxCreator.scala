package com.mhm.epsmi.dummy

import java.util.concurrent.atomic.AtomicInteger

object DummyTxCreator {

  val masterDummyId = new AtomicInteger(1000)

  def dummySpkToAddress(spk: String): String = spk + "-address"

  def createDummyFundingTx(
    confirmations: Int = 1,
    outputSpkOpt: Option[String] = None,
    inputTxid: String = "placeholder-unknown-input-txid",
    coinbase: Boolean = false,
    inputConfirmations: Int = 1): (String, Int, Map[String, Any]) = {
    val dummyId = masterDummyId.getAndIncrement()
    val dummySpk = outputSpkOpt match {
      case Some(outputSpk) => outputSpk
      case None => "deadbeef" + dummyId //scriptpubkey
    }
    val dummyContainingBlock = "blockhash-placeholder" + dummyId
    val containingBlockHeight = dummyId
    val category = if (!coinbase) "receive" else {
      if (confirmations < 1) "orphan"
      else if (confirmations <= 100) "immature"
      else "generate"
    }
    val vin = scala.collection.mutable.Map (
      "txid" -> inputTxid,
      "vout" -> 0,
      "value" -> 1,
      "confirmations" -> inputConfirmations,
    )
    if (coinbase)
      vin.put("coinbase", "nonce")
    val vout = Map (
      "value" -> 1,
      "scriptPubKey" -> """{"hex": dummy_spk}"""
    )

    val dummyTx = Map(
      "txid" -> s"placeholder-test-txid$dummyId",
      "vin" -> vin.toMap,
      "vout" -> vout,
      "address" -> dummySpkToAddress(dummySpk),
      "category" -> category,
      "confirmations" -> confirmations,
      "blockhash" -> dummyContainingBlock,
      "hex" -> s"placeholder-test-txhex$dummyId"
    )

    (dummySpk, containingBlockHeight, dummyTx)
  }

}
