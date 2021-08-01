/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.epsmi.roundtrip.test

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.epsmi.dummymonitor.DummyBtcRpc.{
  toGetTransactionResult,
  toGetTxOutResult,
  toListTransactionsResult,
  toRpcTransaction
}
import com.mhm.epsmi.dummymonitor.DummyTxCreator.{DummyTx, DummyVin}
import com.mhm.epsmi.dummyprotocol.DummyProtocolBtcRpc
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind._
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

case class DummyRoundtripBtcRpc(txList: Seq[DummyTx],
                                utxoSet: Seq[DummyVin]         = Nil,
                                blockHeights: Map[String, Int] = Map())
    extends BitcoindRpcExtendedClient(TestBitcoinSConnector.bitcoindInstance, TestBitcoinSConnector.system) {

  val protocolRpc = DummyProtocolBtcRpc()

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
    protocolRpc.getBlockHeader(headerHash)
  }

  override def getBestBlockHash: Future[DoubleSha256DigestBE] = {
    protocolRpc.getBestBlockHash
  }
}
