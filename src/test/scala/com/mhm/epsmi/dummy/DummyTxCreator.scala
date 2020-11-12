package com.mhm.epsmi.dummy

import java.util.concurrent.atomic.AtomicInteger

import org.bitcoins.core.protocol.transaction.BaseTransaction

object DummyTxCreator {

  val masterDummyId = new AtomicInteger(1000)

  def dummySpkToAddress(spk: String): String = "1FpeH5RojTMpaUS8oreYBRtMpCk1mfVxcf"

  case class DummyVin(
    txId: String,
    vout: Int,
    value: Int,
    confirmations: Int,
    coinbase: Option[String] = None
  )

  case class DummyVout(
    value: Int,
    scriptPubKey: String
  )

  case class DummyTx(
    txId: String,
    vin: DummyVin,
    vout: DummyVout,
    address: String,
    category: String,
    confirmations: Int,
    blockhash: String,
    hex: BaseTransaction
  )

  def createDummyFundingTx(
    confirmations: Int = 1,
    outputSpkOpt: Option[String] = None,
    inputTxid: String = "e0"*32,
    txId: String = "a0"*32,
    coinbase: Boolean = false,
    inputConfirmations: Int = 1,
    hexDifferentiator: Int = 15900): (String, Int, DummyTx) = {
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
    val vin = DummyVin(
      txId = inputTxid,
      vout = 0,
      value = 100,
      inputConfirmations,
      if (coinbase) Some("nonce") else None
    )
    val vout = DummyVout(
      value = 98,
      scriptPubKey = dummySpk
    )

    val dummyTx = DummyTx(
      txId = txId,
      vin = vin,
      vout = vout,
      address = dummySpkToAddress(dummySpk),
      category = category,
      confirmations = confirmations,
      blockhash = dummyContainingBlock,
      hex = BaseTransaction(org.bitcoins.core.number.Int32(0), Nil, Nil, org.bitcoins.core.number.UInt32(hexDifferentiator))
    )

    (dummySpk, containingBlockHeight, dummyTx)
  }

}
