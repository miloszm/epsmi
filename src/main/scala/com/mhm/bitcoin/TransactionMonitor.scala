package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.HashOps
import com.mhm.wallet.DeterministicWallet
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.util.{Failure, Success, Try}

case class Tx4HistoryGen(confirmations: Int, txid: String, blockhash: DoubleSha256DigestBE)

/**
 * @param rpcCli bitcoin core client
 * @param rawMode if true allows extracting input transactions when they are not wallet transactions
 */
class TransactionMonitor(rpcCli: BitcoindRpcExtendedClient, rawMode: Boolean) extends Logging {

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): AddressHistory = {
    logger.info("started buildAddressHistory")
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from(monitoredScriptPubKeys.map(k => HashOps.script2ScriptHash(k) -> HistoryEntry(false, Nil)))
    )
    logger.info(s"initialized address history keys with ${ah.m.keySet.size} keys, head entry is ${ah.m.head}")

    val BATCH_SIZE = 1000

    val walletAddrScripthashes = ah.m.keySet

    def go(skip: Int, obtainedTxids: Set[String]): Unit = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions")
      logger.info(s"obtained ${transactions.size} transactions (skip=$skip)")
      val lastTx = if ((transactions.size < BATCH_SIZE) && skip == 0) Some(transactions.last) else None
      val newTxids = (transactions.flatMap{ tx: ListTransactionsResult =>
        if (tx.txid.isDefined
            && Set("receive", "send", "generate", "immature").contains(tx.category)
            && tx.confirmations.isDefined
            && tx.confirmations.get >= 0
            && tx.txid.isDefined
            && !obtainedTxids.contains(tx.txid.get.hex)
            ){
          logger.debug(s"tx ${tx.txid} checked for category: '${tx.category}', confirmations '${tx.confirmations}' and more")

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
            val tx4HistoryGen = Tx4HistoryGen(
              tx.confirmations.getOrElse(throw new IllegalArgumentException("missing confirmations")),
              tx.txid.map(_.hex).getOrElse(throw new IllegalArgumentException("missing txid")),
              tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))
            )
            val newHistoryElement = generateNewHistoryElement(tx4HistoryGen, txd)
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
    logger.debug("started getInputAndOutputScriptpubkeys")
    val getTx = wrap(rpcCli.getTransaction(txid), "getTransaction")
    val txd: RpcTransaction = wrap(rpcCli.decodeRawTransaction(getTx.hex), "decodeRawTransaction")
    val outputScriptpubkeys: Seq[String] = txd.vout.map(_.scriptPubKey.hex)
    logger.debug(s"got ${outputScriptpubkeys.size} output scriptpubkeys: ${outputScriptpubkeys.mkString("|")}")
    val inputScriptpubkeys = txd.vin.flatMap{ inn =>
      // TODO check for coinbase, don't know how at the moment
      val inputTransactionId = DoubleSha256DigestBE.fromHex(inn.previousOutput.txIdBE.hex)
      logger.debug(inputTransactionId.hex)
      logger.debug(inputTransactionId.flip.hex)
      val resultTry = if (rawMode)
        Try(wrap(rpcCli.getRawTransaction(inputTransactionId), "getRawTransaction")).map(_.hex)
      else
        Try(wrap(rpcCli.getTransaction(inputTransactionId), "getTransaction")).map(_.hex)
      resultTry match {
        case Failure(_) => None
        case Success(r) => Some {
          logger.debug(s"decoding raw transaction ${r.hex}")
          val inputDecoded = wrap(rpcCli.decodeRawTransaction(r), "decodeRawTransaction")
          val script = inputDecoded.vout(inn.previousOutput.vout.toInt).scriptPubKey.hex
          script
        }
      }
    }
    logger.debug(s"got ${inputScriptpubkeys.size} input scriptpubkeys: ${inputScriptpubkeys.mkString("|")}")
    logger.debug(s"finished getInputAndOutputScriptpubkeys with ${inputScriptpubkeys.size} input(s) and ${outputScriptpubkeys.size} output(s)")
    (outputScriptpubkeys, inputScriptpubkeys, txd)
  }

  def generateNewHistoryElement(tx: Tx4HistoryGen, txd: RpcTransaction): HistoryElement = {
    if (tx.confirmations == 0){
      var unconfirmedInput = false
      var totalInputValue = BigDecimal(0)
      for (inn <- txd.vin){
        val utxo = wrap(rpcCli.getTxOut(inn.previousOutput.txIdBE, inn.previousOutput.vout.toInt), "getTxOut")
        totalInputValue = totalInputValue + utxo.value.toBigDecimal
        unconfirmedInput = unconfirmedInput || utxo.confirmations == 0
      }
      val height = if (unconfirmedInput) -1 else 0
      val totalOut = (
        for {out <- txd.vout} yield out.value.toBigDecimal
      ).foldLeft(BigDecimal(0))(_ + _)
      val fee = totalInputValue - totalOut
      HistoryElement(tx.txid, height, fee)
    }
    else {
      val blockHeader: GetBlockHeaderResult = wrap(rpcCli.getBlockHeader(tx.blockhash))
      HistoryElement(tx.txid, blockHeader.height)
    }
  }
}
