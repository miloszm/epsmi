package com.mhm.epsmi.dummy

import com.mhm.connectors.BitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind.{GetTransactionResult, ListTransactionsResult, RpcScriptPubKey, RpcTransaction, RpcTransactionOutput}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionInput, TransactionOutPoint}
import org.bitcoins.core.script.ScriptType.PUBKEYHASH
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.concurrent.Future

class DummyBtcRpc(txList: Seq[Map[String, Any]], utxoSet: Seq[Any], blockHeights: Seq[Int])
  extends BitcoindRpcClient(BitcoinSConnector.bitcoindInstance)(BitcoinSConnector.system){

  def toListTransactionsResult(tx: Map[String, Any]): ListTransactionsResult = {
    val voutMap: Map[String, Any] = tx.get("vout").asInstanceOf[Map[String, Any]]
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
      hex = Transaction.fromHex(tx("hex").asInstanceOf[String])
    )
  }

  def toRpcTransaction(tx: Map[String, Any]): RpcTransaction = {
    val voutMap: Map[String, Any] = tx.get("vout").asInstanceOf[Map[String, Any]]
    val voutValue: Int = voutMap("value").asInstanceOf[Int]
    val voutSpk = voutMap("scriptPubKey").asInstanceOf[String]
    val voutRpcSpk = RpcScriptPubKey("", hex=voutSpk, None, PUBKEYHASH, None)

    val transactionInput = TransactionInput.fromTxidAndVout(
      DoubleSha256DigestBE.fromHex(tx("vin").asInstanceOf[Map[String,Any]]("txid").asInstanceOf[String]),
      UInt32(tx("vin").asInstanceOf[Map[String,Any]]("vout").asInstanceOf[Int])
    )

    val transactionOutput = RpcTransactionOutput(Bitcoins(0), voutValue, voutRpcSpk)

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

  override def listTransactions(account: String, count: Int, skip: Int, includeWatchOnly: Boolean): Future[Vector[ListTransactionsResult]] = {
    Future.successful(txList.slice(skip, skip + count).map(toListTransactionsResult).toVector)
  }

  override def getTransaction(txid: DoubleSha256DigestBE, watchOnly: Boolean): Future[GetTransactionResult] = {
    val tx = txList.map(toGetTransactionResult).filter(_.txid == txid)
    Future.successful(tx.headOption.getOrElse(throw new IllegalArgumentException("tx not found")))
  }

  override def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    val filtered = txList.filter(m => m.get("hex").map(_.asInstanceOf[String]).getOrElse("") == transaction.hex)
    val tx = filtered.headOption.getOrElse(throw new IllegalArgumentException("tx not found"))
    Future.successful(toRpcTransaction(tx))
  }
}
