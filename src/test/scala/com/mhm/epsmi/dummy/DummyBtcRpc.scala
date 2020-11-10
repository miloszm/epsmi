package com.mhm.epsmi.dummy

import com.mhm.connectors.BitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind.ListTransactionsResult
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
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
      address = tx.get("address").map(a => BitcoinAddress(a.asInstanceOf[String])),
      category = tx("category").asInstanceOf[String],
      amount = Bitcoins(0),
      label = None,
      vout = voutValueOpt,
      fee = None,
      confirmations = tx.get("confirmations").map(_.asInstanceOf[Int]),
      trusted = None,
      generated = None,
      blockhash = tx.get("blockhash").map(_.asInstanceOf[String]).map(DoubleSha256DigestBE.fromHex),
      blockindex = None,
      blocktime = None,
      txid = tx.get("txid").map(_.asInstanceOf[String]).map(DoubleSha256DigestBE.fromHex),
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

  override def listTransactions(account: String, count: Int, skip: Int, includeWatchOnly: Boolean): Future[Vector[ListTransactionsResult]] = {
    Future.successful(txList.slice(skip, skip + count).map(toListTransactionsResult).toVector)
  }
}
