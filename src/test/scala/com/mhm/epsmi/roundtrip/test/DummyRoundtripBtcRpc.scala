package com.mhm.epsmi.roundtrip.test

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.epsmi.dummymonitor.DummyBtcRpc.{toGetTransactionResult, toGetTxOutResult, toListTransactionsResult, toRpcTransaction}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.{DummyTx, DummyVin}
import com.mhm.epsmi.dummyprotocol.DummyProtocolBtcRpc
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

case class DummyRoundtripBtcRpc(txList: Seq[DummyTx], utxoSet: Seq[DummyVin] = Nil, blockHeights: Map[String, Int] = Map())
  extends BitcoindRpcExtendedClient(TestBitcoinSConnector.bitcoindInstance, TestBitcoinSConnector.system){

  val protocolRpc = DummyProtocolBtcRpc()

  override def listTransactions(account: String, count: Int, skip: Int, includeWatchOnly: Boolean): Future[Vector[ListTransactionsResult]] = {
    Future.successful(txList.reverse.slice(skip, skip + count).map(toListTransactionsResult).toVector.reverse)
  }

  override def getTransaction(txid: DoubleSha256DigestBE, watchOnly: Boolean): Future[GetTransactionResult] = {
    val tx = txList.filter(_.txId == txid.hex)
    Future.successful(tx.headOption.map(toGetTransactionResult).getOrElse(throw new IllegalArgumentException(s"tx not found $txid")))
  }

  override def decodeRawTransaction(transaction: Transaction): Future[RpcTransaction] = {
    val filtered = txList.filter(m => m.hex.lockTime == transaction.lockTime)
    val tx = filtered.headOption.getOrElse(throw new IllegalArgumentException("tx not found"))
    Future.successful(toRpcTransaction(tx))
  }

  override def getTxOut(txid: DoubleSha256DigestBE, vout: Int, includeMemPool: Boolean = true): Future[GetTxOutResult] = {
    val filtered = utxoSet.filter(m => m.txId == txid.hex && m.vout == vout)
    val u = filtered.headOption.getOrElse(throw new IllegalArgumentException("utxo not found"))
    Future.successful(toGetTxOutResult(u))
  }

  override def getBlockHeader(headerHash: DoubleSha256DigestBE): Future[GetBlockHeaderResult] = {
    protocolRpc.getBlockHeader(headerHash)
  }

  override def getBestBlockHash: Future[DoubleSha256DigestBE] = {
    protocolRpc.getBestBlockHash
  }
}
