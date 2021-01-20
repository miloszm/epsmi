package com.mhm.epsmi.dummymonitor

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.epsmi.dummymonitor.DummyBtcRpc.{
  toGetTransactionResult,
  toGetTxOutResult,
  toListTransactionsResult,
  toRpcTransaction
}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.{DummyTx, DummyVin}
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput}
import org.bitcoins.core.script.ScriptType.PUBKEYHASH
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

object DummyBtcRpc {

  def toListTransactionsResult(tx: DummyTx): ListTransactionsResult = {
    ListTransactionsResult(
      account            = None,
      address            = Some(BitcoinAddress(tx.address)),
      category           = tx.category,
      amount             = Bitcoins(0),
      label              = None,
      vout               = Some(tx.vout.value),
      fee                = None,
      confirmations      = Some(tx.confirmations),
      trusted            = None,
      generated          = None,
      blockhash          = Some(DoubleSha256DigestBE.fromHex(tx.blockhash)),
      blockindex         = None,
      blocktime          = None,
      txid               = Some(DoubleSha256DigestBE.fromHex(tx.txId)),
      walletconflicts    = None,
      time               = UInt32(0),
      timereceived       = None,
      comment            = None,
      to                 = None,
      otheraccount       = None,
      bip125_replaceable = "",
      abandoned          = None
    )
  }

  def toGetTransactionResult(tx: DummyTx): GetTransactionResult = {
    GetTransactionResult(
      amount             = Bitcoins(0),
      fee                = None,
      confirmations      = tx.confirmations,
      generated          = None,
      blockhash          = Some(DoubleSha256DigestBE.fromHex(tx.blockhash)),
      blockindex         = None,
      blocktime          = None,
      txid               = DoubleSha256DigestBE.fromHex(tx.txId),
      walletconflicts    = Vector(),
      time               = UInt32(0),
      timereceived       = UInt32(0),
      comment            = None,
      to                 = None,
      bip125_replaceable = "",
      // NOTE - make sure the below agrees with the real API behaviour
      details = Vector(TransactionDetails(None, None, None, tx.category, Bitcoins(0), 0, None, None)),
      hex     = tx.hex
    )
  }

  def toGetRawTransactionResult(tx: DummyTx): GetRawTransactionResult = {
    val voutValue: Int = tx.vout.value
    val voutSpk        = tx.vout.scriptPubKey
    val voutRpcSpk     = RpcScriptPubKey("", hex = voutSpk, None, PUBKEYHASH, None)

    val transactionInput = GetRawTransactionVin(
      txid        = Some(DoubleSha256DigestBE.fromHex(tx.vin.txId)),
      vout        = Some(tx.vin.vout),
      scriptSig   = None,
      sequence    = None,
      txinwitness = None
    )

    val transactionOutput = RpcTransactionOutput(Bitcoins(voutValue), voutValue, voutRpcSpk)

    GetRawTransactionResult(
      in_active_blockchain = None,
      hex                  = tx.hex,
      txid                 = DoubleSha256DigestBE.fromHex(tx.txId),
      hash                 = DoubleSha256DigestBE.fromHex(tx.txId),
      size                 = 0,
      vsize                = 0,
      version              = 0,
      locktime             = UInt32(0),
      vin                  = Vector(transactionInput),
      vout                 = Vector(transactionOutput),
      blockhash            = Some(DoubleSha256DigestBE.fromHex(tx.blockhash)),
      confirmations        = Some(tx.confirmations),
      time                 = None,
      blocktime            = None
    )
  }

  def toRpcTransaction(tx: DummyTx): RpcTransaction = {
    val voutValue: Int = tx.vout.value
    val voutSpk        = tx.vout.scriptPubKey
    val voutRpcSpk     = RpcScriptPubKey("", hex = voutSpk, None, PUBKEYHASH, None)

    val transactionInput =
      TransactionInput.fromTxidAndVout(DoubleSha256DigestBE.fromHex(tx.vin.txId), UInt32(tx.vin.vout))

    val transactionOutput = RpcTransactionOutput(Bitcoins(voutValue), voutValue, voutRpcSpk)

    RpcTransaction(
      txid     = DoubleSha256DigestBE.fromHex(tx.txId),
      hash     = DoubleSha256DigestBE.fromHex(tx.txId),
      version  = 0,
      size     = 0,
      vsize    = 0,
      locktime = UInt32(0),
      vin      = Vector(transactionInput),
      vout     = Vector(transactionOutput),
      hex      = None
    )
  }

  def toGetTxOutResult(u: DummyVin): GetTxOutResult = {
    GetTxOutResult(
      //value = Bitcoins(BigDecimal(u("value").asInstanceOf[Int])/BigDecimal(100000000)),
      value         = Bitcoins(u.value),
      confirmations = u.confirmations,
      bestblock     = null,
      scriptPubKey  = null,
      coinbase      = false
    )
  }
}

case class DummyBtcRpc(txList: Seq[DummyTx], utxoSet: Seq[DummyVin] = Nil, blockHeights: Map[String, Int] = Map())
    extends BitcoindRpcExtendedClient(TestBitcoinSConnector.bitcoindInstance, TestBitcoinSConnector.system) {

  override def listTransactions(account: String,
                                count: Int,
                                skip: Int,
                                includeWatchOnly: Boolean): Future[Vector[ListTransactionsResult]] = {
    Future.successful(txList.reverse.slice(skip, skip + count).map(toListTransactionsResult).toVector.reverse)
  }

  override def getTransaction(txid: DoubleSha256DigestBE, watchOnly: Boolean): Future[GetTransactionResult] = {
    val tx = txList.filter(_.txId == txid.hex)
    Future.successful(
      tx.headOption.map(toGetTransactionResult).getOrElse(throw new IllegalArgumentException(s"tx not found $txid"))
    )
  }

  override def getRawTransaction(txid: DoubleSha256DigestBE,
                                 blockhash: Option[DoubleSha256DigestBE]): Future[GetRawTransactionResult] = {
    super.getRawTransaction(txid, blockhash)
  }

  override def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    val filtered = txList.filter(m => m.hex.lockTime == transaction.lockTime)
    val tx       = filtered.headOption.getOrElse(throw new IllegalArgumentException("tx not found"))
    Future.successful(toRpcTransaction(tx))
  }

  override def getTxOut(txid: DoubleSha256DigestBE,
                        vout: Int,
                        includeMemPool: Boolean = true): Future[GetTxOutResult] = {
    val filtered = utxoSet.filter(m => m.txId == txid.hex && m.vout == vout)
    val u        = filtered.headOption.getOrElse(throw new IllegalArgumentException("utxo not found"))
    Future.successful(toGetTxOutResult(u))
  }

  override def getBlockHeader(headerHash: DoubleSha256DigestBE): Future[GetBlockHeaderResult] = {
    val height = blockHeights.getOrElse(headerHash.hex, throw new IllegalArgumentException("block header not found"))
    Future.successful(
      GetBlockHeaderResult(
        null,
        0,
        height,
        0,
        org.bitcoins.core.number.Int32(0),
        null,
        UInt32(0),
        UInt32(0),
        UInt32(0),
        UInt32(0),
        0,
        "",
        None,
        None
      )
    )
  }
}
