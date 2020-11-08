package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.HashOps
import com.mhm.wallet.DeterministicWallet
import org.bitcoins.commons.jsonmodels.bitcoind.{ListTransactionsResult, RpcTransaction}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.util.{Failure, Success, Try}

class TransactionMonitor(rpcCli: BitcoindRpcExtendedClient) {

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): AddressHistory = {
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from(monitoredScriptPubKeys.map(k => HashOps.script2ScriptHash(k) -> HistoryEntry(false, Nil)))
    )

    val BATCH_SIZE = 1000

    val walletAddrScripthashes = ah.m.keySet

    def go(skip: Int, obtainedTxids: Set[String]): Unit = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions 0")
      val lastTx = if ((transactions.size < BATCH_SIZE) && skip == 0) Some(transactions.last) else None
      val newTxids = (transactions.flatMap{ tx: ListTransactionsResult =>
        if (tx.txid.isDefined
            && Set("receive", "send", "generate", "immature").contains(tx.category)
            && tx.confirmations.isDefined
            && tx.confirmations.get >= 0
            && tx.txid.isDefined
            && !obtainedTxids.contains(tx.txid.get.hex)
            ){

          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val outputScriptHashes = outputScriptpubkeys.map(HashOps.script2ScriptHash)
          val shToAddOut = walletAddrScripthashes.intersect(outputScriptHashes.toSet)
          val inputScriptHashes = inputScriptpubkeys.map(HashOps.script2ScriptHash)
          val shToAddIn = walletAddrScripthashes.intersect(inputScriptHashes.toSet)
          val shToAdd = shToAddIn ++ shToAddOut
          if (shToAdd.isEmpty) None else {
            for(wal <- deterministicWallets){
              val overrunDepths = wal.haveScriptpubkeysOverrunGaplimit(outputScriptpubkeys)
              if (overrunDepths.nonEmpty) throw new IllegalStateException("not enough addresses imported, see transactionmonitor.py line 155")
            }
            val newHistoryElement = generateNewHistoryElement(tx, txd)
            tx.txid.map(_.hex)
          }
        }
        else None
      }).toSet


      //val overrunDepths = deterministicWallets.map(_.)

      if (transactions.size == BATCH_SIZE) go(skip + BATCH_SIZE, obtainedTxids ++ newTxids)
      else ()
    }
    go(skip = 0, obtainedTxids = Set())

    ah
  }

  def getInputAndOutputScriptpubkeys(txid: DoubleSha256DigestBE): (Seq[String], Seq[String], RpcTransaction) = {
    val getTx = wrap(rpcCli.getTransaction(txid), "getTransaction 1")
    val txd: RpcTransaction = wrap(rpcCli.decodeRawTransaction(getTx.hex), "decodeRawTransaction 1")
    val outputScriptpubkeys: Seq[String] = txd.vout.map(_.scriptPubKey.hex)
    val inputScriptpubkeys = txd.vin.flatMap{ inn =>
      // TODO check for coinbase, don't know how at the moment
      val xTry = Try(wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(inn.previousOutput.txId.hex)), "getTransaction 1"))
      xTry match {
        case Failure(e) => None
        case Success(r) => Some {
          wrap(rpcCli.decodeRawTransaction(r.hex), "decodeRawTransaction 1").txid.hex
        }
      }
    }
    (outputScriptpubkeys, inputScriptpubkeys, txd)
  }

  def generateNewHistoryElement(tx: ListTransactionsResult, txd: RpcTransaction): Unit = {
    if (tx.confirmations.contains(0)){
      var unconfirmedInput = false
      var totalInputValue: Bitcoins = Bitcoins(0)
      for (inn <- txd.vin){
        val utxo = wrap(rpcCli.getTxOut(inn.previousOutput.txIdBE, inn.previousOutput.vout.toInt), "getTxOut 2")
        totalInputValue = Bitcoins((totalInputValue + utxo.value).satoshis)
        unconfirmedInput = unconfirmedInput || utxo.confirmations == 0
      }
    }
    else {

    }
  }
}
