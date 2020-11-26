package com.mhm.epsmi.dummymonitor

import java.util.concurrent.atomic.AtomicInteger

import org.bitcoins.core.protocol.transaction.BaseTransaction

object DummyTxCreator {

  lazy val masterDummyId = new AtomicInteger(1000)

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
    coinbase: Boolean = false,
    inputConfirmations: Int = 1,
    masterId: Int = -1): (String, Int, DummyTx) = {
    val dummyId = if (masterId == -1) masterDummyId.getAndIncrement() else masterId
    val dummySpk = outputSpkOpt match {
      case Some(outputSpk) => outputSpk
      case None => f"$dummyId%04x"*16
    }
    val dummyContainingBlock = f"${dummyId+1000}%04x"*16
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
      txId = f"${dummyId+2000}%04x"*16,
      vin = vin,
      vout = vout,
      address = dummySpkToAddress(dummySpk),
      category = category,
      confirmations = confirmations,
      blockhash = dummyContainingBlock,
      hex = BaseTransaction(org.bitcoins.core.number.Int32(0), Nil, Nil, org.bitcoins.core.number.UInt32(dummyId))
    )

    (dummySpk, containingBlockHeight, dummyTx)
  }

}
