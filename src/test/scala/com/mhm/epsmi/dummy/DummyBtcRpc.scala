package com.mhm.epsmi.dummy

import com.mhm.connectors.{BitcoinSConnector, BitcoindRpcExtendedClient}
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{BaseTransaction, Transaction, TransactionInput}
import org.bitcoins.core.script.ScriptType.PUBKEYHASH
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}

import scala.concurrent.Future

class DummyBtcRpc(txList: Seq[Map[String, Any]], utxoSet: Seq[Map[String, Any]] = Nil, blockHeights: Map[String, Int] = Map())
  extends BitcoindRpcExtendedClient(BitcoinSConnector.bitcoindInstance, BitcoinSConnector.system){

  def toListTransactionsResult(tx: Map[String, Any]): ListTransactionsResult = {
    val voutMap: Map[String, Any] = tx("vout").asInstanceOf[Map[String, Any]]
    val voutValueOpt: Option[Int] = voutMap.get("value").map(_.asInstanceOf[Int])

    ListTransactionsResult(
      account = None,
      address = Some(BitcoinAddress(tx("address").asInstanceOf[String])),
      category = tx("category").asInstanceOf[String],
      amount = Bitcoins(0),
      label = None,
      vout = voutValueOpt,
      fee = None,
      confirmations = Some(tx("confirmations").asInstanceOf[Int]),
      trusted = None,
      generated = None,
      blockhash = Some(DoubleSha256DigestBE.fromHex(tx("blockhash").asInstanceOf[String])),
      blockindex = None,
      blocktime = None,
      txid = Some(DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String])),
      walletconflicts = None,
      time = UInt32(0),
      timereceived = None,
      comment = None,
      to = None,
      otheraccount = None,
      bip125_replaceable = "",
      abandoned = None
    )
  }

  def toGetTransactionResult(tx: Map[String, Any]): GetTransactionResult = {
    GetTransactionResult(
      amount = Bitcoins(0),
      fee = None,
      confirmations = tx("confirmations").asInstanceOf[Int],
      generated = None,
      blockhash = Some(DoubleSha256DigestBE.fromHex(tx("blockhash").asInstanceOf[String])),
      blockindex = None,
      blocktime = None,
      txid = DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String]),
      walletconflicts = Vector(),
      time = UInt32(0),
      timereceived = UInt32(0),
      comment = None,
      to = None,
      bip125_replaceable = "",
      details = Vector(),
      hex = tx("hex").asInstanceOf[BaseTransaction]
    )
  }

  def toGetRawTransactionResult(tx: Map[String, Any]): GetRawTransactionResult = {
    val voutMap: Map[String, Any] = tx.get("vout").asInstanceOf[Map[String, Any]]
    val voutValue: Int = voutMap("value").asInstanceOf[Int]
    val voutSpk = voutMap("scriptPubKey").asInstanceOf[String]
    val voutRpcSpk = RpcScriptPubKey("", hex=voutSpk, None, PUBKEYHASH, None)

    val transactionInput = GetRawTransactionVin(
      txid = Some(DoubleSha256DigestBE.fromHex(tx("vin").asInstanceOf[Map[String,Any]]("txid").asInstanceOf[String])),
      vout = Some(tx("vin").asInstanceOf[Map[String,Any]]("vout").asInstanceOf[Int]),
      scriptSig = None,
      sequence = None,
      txinwitness = None
    )

    val transactionOutput = RpcTransactionOutput(Bitcoins(voutValue), voutValue, voutRpcSpk)

    GetRawTransactionResult(
      in_active_blockchain = None,
      hex = Transaction.fromHex(tx("hex").asInstanceOf[String]),
      txid = DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String]),
      hash = DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String]),
      size = 0,
      vsize = 0,
      version = 0,
      locktime = UInt32(0),
      vin = Vector(transactionInput),
      vout = Vector(transactionOutput),
      blockhash = Some(DoubleSha256DigestBE.fromHex(tx("blockhash").asInstanceOf[String])),
      confirmations = Some(tx("confirmations").asInstanceOf[Int]),
      time = None,
      blocktime = None
    )
  }

  def toRpcTransaction(tx: Map[String, Any]): RpcTransaction = {
    val voutMap: Map[String, Any] = tx("vout").asInstanceOf[Map[String, Any]]
    val voutValue: Int = voutMap("value").asInstanceOf[Int]
    val voutSpk = voutMap("scriptPubKey").asInstanceOf[String]
    val voutRpcSpk = RpcScriptPubKey("", hex=voutSpk, None, PUBKEYHASH, None)

    val transactionInput = TransactionInput.fromTxidAndVout(
      DoubleSha256DigestBE.fromHex(tx("vin").asInstanceOf[Map[String,Any]]("txid").asInstanceOf[String]),
      UInt32(tx("vin").asInstanceOf[Map[String,Any]]("vout").asInstanceOf[Int])
    )

    val transactionOutput = RpcTransactionOutput(Bitcoins(voutValue), voutValue, voutRpcSpk)

    RpcTransaction(
      txid = DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String]),
      hash = DoubleSha256DigestBE.fromHex(tx("txid").asInstanceOf[String]),
      version = 0,
      size = 0,
      vsize = 0,
      locktime = UInt32(0),
      vin = Vector(transactionInput),
      vout = Vector(transactionOutput),
      hex = None
    )
  }

  def toGetTxOutResult(u: Map[String, Any]): GetTxOutResult = {
    GetTxOutResult(
      //value = Bitcoins(BigDecimal(u("value").asInstanceOf[Int])/BigDecimal(100000000)),
      value = Bitcoins(u("value").asInstanceOf[Int]),
      confirmations = u("confirmations").asInstanceOf[Int],
      bestblock = null,
      scriptPubKey = null,
      coinbase = false
    )
  }

  override def listTransactions(account: String, count: Int, skip: Int, includeWatchOnly: Boolean): Future[Vector[ListTransactionsResult]] = {
    Future.successful(txList.slice(skip, skip + count).map(toListTransactionsResult).toVector)
  }

  override def getTransaction(txid: DoubleSha256DigestBE, watchOnly: Boolean): Future[GetTransactionResult] = {
    val converted = txList.map(toGetTransactionResult)
    val tx = converted.filter(_.txid == txid)
    Future.successful(tx.headOption.getOrElse(throw new IllegalArgumentException("tx not found")))
  }

  override def getRawTransaction(txid: DoubleSha256DigestBE, blockhash: Option[DoubleSha256DigestBE]): Future[GetRawTransactionResult] = {
    super.getRawTransaction(txid, blockhash)
  }

  override def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    val filtered = txList.filter(m => m("hex").asInstanceOf[BaseTransaction].lockTime == transaction.lockTime)
    val tx = filtered.headOption.getOrElse(throw new IllegalArgumentException("tx not found"))
    Future.successful(toRpcTransaction(tx))
  }

  override def getTxOut(txid: DoubleSha256DigestBE, vout: Int, includeMemPool: Boolean = true): Future[GetTxOutResult] = {
    val filtered = utxoSet.filter(m => m("txid").asInstanceOf[String] == txid.hex && m("vout").asInstanceOf[Int] == vout)
    val u = filtered.headOption.getOrElse(throw new IllegalArgumentException("utxo not found"))
    Future.successful(toGetTxOutResult(u))
  }

  override def getBlockHeader(headerHash: DoubleSha256DigestBE): Future[GetBlockHeaderResult] = {
    val height = blockHeights.getOrElse(headerHash.hex, throw new IllegalArgumentException("block header not found"))
    Future.successful(
      GetBlockHeaderResult(null, 0, height, 0, org.bitcoins.core.number.Int32(0), null, UInt32(0), UInt32(0), UInt32(0), UInt32(0), 0, "", None, None)
    )
  }
}
