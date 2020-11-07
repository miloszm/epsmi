package com.mhm.bitcoin

import com.mhm.util.HashOps
import com.mhm.wallet.DeterministicWallet
import org.bitcoins.commons.jsonmodels.bitcoind.{GetTransactionResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.rpc.client.common.BitcoindRpcClient

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

object TransactionMonitor {

  def buildAddressHistory(
    rpcCli: BitcoindRpcClient,
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): AddressHistory = {
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from(monitoredScriptPubKeys.map(k => HashOps.script2ScriptHash(k) -> HistoryEntry(false, Nil)))
    )

    val BATCH_SIZE = 1000

    val walletAddrScripthashes = ah.m.keySet

    def go(skip: Int, obtainedTxids: Set[String]): Unit = {
      val transactions = Await.result(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), 20.seconds)
      val lastTx = if ((transactions.size < BATCH_SIZE) && skip == 0) Some(transactions.last) else None
      val newTxids = (transactions.flatMap{ tx: ListTransactionsResult =>
        if (tx.txid.isDefined
            && Set("receive", "send", "generate", "immature").contains(tx.category)
            && tx.confirmations.isDefined
            && tx.confirmations.get >= 0
            && tx.txid.isDefined
            && !obtainedTxids.contains(tx.txid.get.hex)
            ){

          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(rpcCli, tx.txid.get)
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
            val newHistoryElement = generateNewHistoryElement(rpcCli, tx, txd)
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

  def getInputAndOutputScriptpubkeys(rpcCli: BitcoindRpcClient, txid: DoubleSha256DigestBE): (Seq[String], Seq[String], RpcTransaction) = {
    val getTx = Await.result(rpcCli.getTransaction(txid), 20.seconds)
    val txd: RpcTransaction = Await.result(rpcCli.decodeRawTransaction(getTx.hex), 20.seconds)
    val outputScriptpubkeys: Seq[String] = txd.vout.map(_.scriptPubKey.hex)
    val inputScriptpubkeys = txd.vin.flatMap{ inn =>
      // TODO check for coinbase, don't know how at the moment
      val xTry = Try(Await.result(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(inn.previousOutput.txId.hex)), 20.seconds))
      xTry match {
        case Failure(e) => None
        case Success(r) => Some {
          Await.result(rpcCli.decodeRawTransaction(r.hex), 20.seconds).txid.hex
        }
      }
    }
    (outputScriptpubkeys, inputScriptpubkeys, txd)
  }

  def generateNewHistoryElement(rpcCli: BitcoindRpcClient, tx: ListTransactionsResult, txd: RpcTransaction): Unit = {
    if (tx.confirmations.contains(0)){
      var unconfirmedInput = false
      var totalInputValue: Bitcoins = Bitcoins(0)
      for (inn <- txd.vin){
        val utxo = Await.result(rpcCli.getTxOut(inn.previousOutput.txIdBE, inn.previousOutput.vout.toInt), 20.seconds)
        totalInputValue = Bitcoins((totalInputValue + utxo.value).satoshis)
        unconfirmedInput = unconfirmedInput || utxo.confirmations == 0
      }
    }
    else {

    }
  }
}
