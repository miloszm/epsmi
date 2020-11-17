package com.mhm.bitcoin

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.EpsmiDataOps.{optAddr2Str, optSha2Sha, optSha2Str}
import com.mhm.util.{EpsmiDataOps, HashOps}
import com.mhm.util.HashOps.script2ScriptHash
import com.mhm.wallet.DeterministicWallet
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.Predef.Set
import scala.annotation.tailrec
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.jdk.CollectionConverters.{ConcurrentMapHasAsScala, SetHasAsScala}
import scala.util.{Failure, Success, Try}

case class Tx4HistoryGen(confirmations: Int, txid: String, blockhash: DoubleSha256DigestBE)
case class TxidAddress(txid: String, address: String)
case class LastKnown(lastKnownTx: Option[TxidAddress])

/**
 * @param rpcCli bitcoin core client
 * @param nonWalletAllowed if true allows extracting input transactions when they are not wallet transactions
 */
class TransactionMonitor(rpcCli: BitcoindRpcExtendedClient, nonWalletAllowed: Boolean, initLastKnown: LastKnown = LastKnown(None)) extends Logging {
  val ConfirmationsSafeFromReorg = 100
  val BATCH_SIZE = 1000

  val lastKnown = new AtomicReference[LastKnown](initLastKnown)

  val unconfirmedTxes: ConcurrentHashMap[String,Seq[String]] = new java.util.concurrent.ConcurrentHashMap[String, Seq[String]]()

  val reorganizableTxes = ListBuffer[(String, String, Int, Seq[String])]()

  def isTxHistoryEligible(tx: ListTransactionsResult, obtainedTxids: Set[String]): Boolean = {
    tx.txid.isDefined && Set("receive", "send", "generate", "immature").contains(tx.category) &&
    (tx.confirmations.isDefined && tx.confirmations.get >= 0) &&
    !obtainedTxids.contains(tx.txid.get.hex)
  }

  case class BuildAddressHistoryResult(
    addressHistory: AddressHistory,
    lastKnown: Option[TxidAddress]
  )

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): BuildAddressHistoryResult = {
    logger.debug("started buildAddressHistory")
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from[String, HistoryEntry](monitoredScriptPubKeys.map(k => (script2ScriptHash(k), HistoryEntry(false, Nil))))
    )
    logger.trace(s"initialized address history keys with ${ah.m.keySet.size} keys, head entry is ${ah.m.head}")

    val walletAddrScripthashes = ah.m.keySet

    def go(skip: Int, obtainedTxids: Set[String]): Vector[ListTransactionsResult] = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions")
      logger.trace(s"obtained ${transactions.size} transactions (skip=$skip)")
      val lastTx = if ((transactions.size < BATCH_SIZE) && skip == 0) Some(transactions.last) else None
      val newTxids = (transactions.flatMap{ tx: ListTransactionsResult =>
        if (isTxHistoryEligible(tx, obtainedTxids)){
          logger.trace(s"tx ${tx.txid} checked for category: '${tx.category}', confirmations '${tx.confirmations}' and more")
          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val shToAddOut = walletAddrScripthashes.intersect(outputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAddIn = walletAddrScripthashes.intersect(inputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAdd = shToAddIn ++ shToAddOut
          logger.trace(s"${shToAdd.size} scripthashes to add")
          if (shToAdd.isEmpty) None else {
            for(wal <- deterministicWallets){
              val overrunDepths = wal.haveScriptpubkeysOverrunGaplimit(outputScriptpubkeys)
              if (overrunDepths.nonEmpty) throw new IllegalStateException("not enough addresses imported, see transactionmonitor.py line 155")
            }
            val tx4HistoryGen = Tx4HistoryGen(tx.confirmations.get, tx.txid.map(_.hex).get, tx.blockhash.get)
            val newHistoryElement = generateNewHistoryElement(tx4HistoryGen, txd)
            shToAdd.foreach{ scriptHash =>
              logger.trace(s"adding history element (${newHistoryElement.txHash}, ${newHistoryElement.height}) to $scriptHash")
              ah.m.get(scriptHash) match {
                case Some(he: HistoryEntry) => ah.m.put(scriptHash, he.copy(history = he.history :+ newHistoryElement))
                case None => ah.m.put(scriptHash, HistoryEntry(false, Seq(newHistoryElement)))
              }
            }
            tx.txid.map(_.hex)
          }
        }
        else None
      }).toSet


      //val overrunDepths = deterministicWallets.map(_.)

      if (transactions.size == BATCH_SIZE) go(skip + BATCH_SIZE, obtainedTxids ++ newTxids)
      else (transactions)
    }
    val ret = go(skip = 0, obtainedTxids = Set())

    ah.m.keys.foreach{ sh =>
      ah.m.updateWith(sh)(_.map(sortAddressHistoryList))
    }

    val lastKnown = ret.lastOption.map{ last =>TxidAddress(EpsmiDataOps.optSha2Str(last.txid), EpsmiDataOps.optAddr2Str(last.address)) }

    logger.debug(s"finished buildAddressHistory, history size = ${ah.m.size}")
    BuildAddressHistoryResult(ah, lastKnown)
  }

  def getInputAndOutputScriptpubkeys(txid: DoubleSha256DigestBE): (Seq[String], Seq[String], RpcTransaction) = {
//    logger.debug("started getInputAndOutputScriptpubkeys")
    val getTx = wrap(rpcCli.getTransaction(txid), "getTransaction")
    val txd: RpcTransaction = wrap(rpcCli.decodeRawTransaction(getTx.hex), "decodeRawTransaction")
    val outputScriptpubkeys: Seq[String] = txd.vout.map(_.scriptPubKey.hex)
//    logger.debug(s"got ${outputScriptpubkeys.size} output scriptpubkeys: ${outputScriptpubkeys.mkString("|")}")
    val inputScriptpubkeys = txd.vin.flatMap{ inn =>
      // TODO check for coinbase, don't know how at the moment
      val inputTransactionId = DoubleSha256DigestBE.fromHex(inn.previousOutput.txIdBE.hex)
      val resultTry = if (nonWalletAllowed)
        Try(wrap(rpcCli.getRawTransaction(inputTransactionId), "getRawTransaction")).map(_.hex)
      else
        Try(wrap(rpcCli.getTransaction(inputTransactionId), "getTransaction")).map(_.hex)
      resultTry match {
        case Failure(_) => None
        case Success(r) => Some {
//          logger.debug(s"decoding raw transaction ${r.hex}")
          val inputDecoded = wrap(rpcCli.decodeRawTransaction(r), "decodeRawTransaction")
          val script = inputDecoded.vout(inn.previousOutput.vout.toInt).scriptPubKey.hex
          script
        }
      }
    }
//    logger.debug(s"got ${inputScriptpubkeys.size} input scriptpubkeys: ${inputScriptpubkeys.mkString("|")}")
//    logger.debug(s"finished getInputAndOutputScriptpubkeys with ${inputScriptpubkeys.size} input(s) and ${outputScriptpubkeys.size} output(s)")
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


  case class CheckForNewTxsResult(
    lastKnown: Option[TxidAddress],
    newFound: Set[String],
    newHistoryElements: ArrayBuffer[(String, HistoryElement)] = ArrayBuffer(),
    newUnconfirmed: ArrayBuffer[(String, String)] = ArrayBuffer(),
    newReorganizable: ArrayBuffer[(String, String, Int, Seq[String])] = ArrayBuffer()
  )

  /**
   * @return set of updated scripthashes
   */
  def checkForNewTxs(ah: AddressHistory, lastKnownTx: Option[TxidAddress]): CheckForNewTxsResult = {
    logger.debug("started checkForNewTxs")
    val MaxTxRequestCount = 256
    val txRequestCount = 2
    val maxAttempts = 8 // log base 2 of 256

    @tailrec
    def go(attempt: Int, maxAttempts: Int, count: Int, v: Vector[ListTransactionsResult], lastKnownTx: Option[TxidAddress]): (Int, Vector[ListTransactionsResult]) = {
      if (attempt == maxAttempts) (0, v)
      else {
        val transactions = wrap(rpcCli.listTransactions("*", count, 0, includeWatchOnly = true), "listTransactions").reverse
        lastKnownTx match {
          case None => (transactions.size, transactions)
          case Some(ln) =>
            val found = transactions.zipWithIndex.find{ case (t, _) => optSha2Str(t.txid) == ln.txid && optAddr2Str(t.address) == ln.address }
            found match {
              case Some((_, recentTxIndex)) => (recentTxIndex, transactions)
              case None => go(attempt+1, maxAttempts, count * 2, transactions, lastKnownTx)
            }
        }
      }
    }
    val (recentTxIndex, ret) = go(0, maxAttempts, 2, Vector(), lastKnownTx)
    val newLastKnownTx = ret.headOption.map { h => TxidAddress(optSha2Str(h.txid), optAddr2Str(h.address)) }
    val result = if (recentTxIndex == 0){
      CheckForNewTxsResult(newLastKnownTx, Set())
    } else {
      val newTxs = ret.slice(0, recentTxIndex).reverse
      val relevantTxs = newTxs
        .filter(_.txid.isDefined)
        .filter(tx => Set("receive", "send", "generate", "immature").contains(tx.category))
        .filter(_.confirmations.getOrElse(0) >= 0)
      val r = CheckForNewTxsResult(newLastKnownTx, Set())
      relevantTxs.foreach { tx =>
          val txid = optSha2Str(tx.txid)
          val blockhash = optSha2Sha(tx.blockhash)
          val (outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val matchingScripthashes = (outputScriptpubkeys ++ inputScriptpubkeys)
            .map(HashOps.script2ScriptHash).filter(ah.m.keySet.contains)
          val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(
            tx.confirmations.getOrElse(throw new IllegalArgumentException("missing confirmations")),
            txid,
            blockhash
          ), txd)
          matchingScripthashes.foreach {ms =>
            r.newHistoryElements.addOne((ms, newHistoryElement))
            if (newHistoryElement.height <= 0)
              r.newUnconfirmed.addOne((txid, ms))
          }
          if (tx.confirmations.getOrElse(-1) > 0)
            r.newReorganizable.addOne((txid, blockhash.hex, newHistoryElement.height, matchingScripthashes))
      }
      r
    }
    logger.debug(s"finished checkForNewTxs, found ${result.newFound.size} new tx(s)")
    result
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForConfirmations(ah: AddressHistory): Set[String] = {

    val removedFromMempool: Seq[(String, Seq[String])] = (for {(txid, shs) <- unconfirmedTxes.asScala.toList} yield {
      val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
      logger.debug(s"uc_txid=$txid => $tx")
      val blockHeightOpt = if (tx.confirmations == 0) None // still unconfirmed
      else if (tx.confirmations > 0){
        logger.info(s"Trnsaction confirmed: $txid")
        val block = wrap(rpcCli.getBlockHeader(tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))))
        Some(block.height)
      } else {
        logger.warn(s"Transaction conflicted: $txid")
        None
      }
      if (tx.confirmations > 0) {
        shs.foreach {sh =>
          //delete the old unconfirmed entry in address_history
          ah.m.get(sh).foreach { he =>
            he.copy(history = he.history.filterNot(_.txHash == txid))
          }
        }
        reorganizableTxes.addOne((txid, tx.blockhash.map(_.hex).getOrElse(throw new IllegalArgumentException("missing blockhash")), blockHeightOpt.get, shs))
        Some((txid, shs))
      }
      else
        None
    }).flatten

    removedFromMempool.foreach{ case (txid,_) =>
      unconfirmedTxes.remove(txid)
    }

    val updatedScriptHashes = removedFromMempool.map{case (_, shs) => shs.toSet}.foldLeft(Set[String]())(_ ++ _)
    updatedScriptHashes
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForReorganizations(ah: AddressHistory): Set[String] = {
    val updatedScripthashes = scala.collection.mutable.Set[String]()
    val removedFromReorganizableTxes = (for {reorgableTx@(txid, blockhash, height, matchingShs) <- reorganizableTxes} yield {
      val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
      if (tx.confirmations >= ConfirmationsSafeFromReorg){
        logger.debug(s"Transaction considered safe from reorg: $txid")
        Some(reorgableTx)
      }
      else if (tx.confirmations < 1){
        updatedScripthashes.addAll(matchingShs)
        if (tx.confirmations == 0){
          // transaction became unconfirmed in reorg
          logger.info(s"Transaction was reorg'd out: $txid")
          unconfirmedTxes.put(txid, unconfirmedTxes.getOrDefault(txid, Nil) ++ matchingShs)
          if (tx.details.head.category != "orphan"){ // TODO not sure why details is a vector and if this category extraction is correct here
            val txd = wrap(rpcCli.decodeRawTransaction(tx.hex))
            val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(tx.confirmations, tx.txid.hex, tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))), txd)
            matchingShs.foreach { ms =>
              ah.m.get(ms) match {
                case Some(he) => ah.m.put(ms, he.copy(history = he.history :+ newHistoryElement))
                case None => ah.m.put(ms, HistoryEntry(false, Seq(newHistoryElement)))
              }
            }
          }
          Some(reorgableTx)
        } else { // confirmations < 0
          logger.info(s"Transaction was double spent! $txid")
          Some(reorgableTx)
        }
      } else if (!tx.blockhash.map(_.hex).contains(blockhash)){
        val block = wrap(rpcCli.getBlockHeader(tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))))
        if (block.height == height) { //reorg but height is the same
          logger.debug(s"Transaction was reorg'd but still confirmed at same height: $txid")
          None
        }
        else {
          //reorged but still confirmed at a different height
          updatedScripthashes.addAll(matchingShs)
          logger.debug("Transaction was reorg'd but still confirmed to a new block and different height: $txid")
          //update history with the new height
          matchingShs.foreach { ms =>
            ah.m.get(ms).foreach { he => ah.m.put(ms, he.copy(history = he.history.map(e =>
              if (e.txHash == txid) e.copy(height = block.height) else e )
            )) }
          }
          reorganizableTxes.addOne(reorgableTx)
          Some(reorgableTx)
        }
      } else {
        None
      }
    }).flatten

    removedFromReorganizableTxes.foreach{ rtx =>
      reorganizableTxes.remove(reorganizableTxes.indexOf(rtx))
    }

    updatedScripthashes.toSet
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForUpdatedTxs(ah: AddressHistory): Set[String] = {
    logger.debug("started checkForUpdatedTxs")
    val updatedScripthashes = /*checkForNewTxs(ah, None) ++ */ checkForConfirmations(ah) ++ checkForReorganizations(ah)
    updatedScripthashes.foreach{ ush =>
      ah.m.updateWith(ush)(_.map(sortAddressHistoryList))
    }
    if (updatedScripthashes.nonEmpty) {
      logger.debug(s"unconfirmed txes = ${unconfirmedTxes.keySet().asScala.mkString("|")}")
      logger.debug(s"reorganizable_txes = ${reorganizableTxes.mkString("|")}")
      logger.debug(s"updated_scripthashes = ${updatedScripthashes.mkString("|")}")
    }
    val updated = updatedScripthashes.filter(sh => ah.m.contains(sh) && ah.m(sh).subscribed)
    logger.debug(s"finished checkForUpdatedTxs, updated size = ${updated.size}")
    updated
  }

  def sortAddressHistoryList(historyEntry: HistoryEntry): HistoryEntry = {
    val unconfirmedTxs = historyEntry.history.filter(_.height <= 0)
    val confirmedTxs = historyEntry.history.filter(_.height > 0)
    val sortedConfirmedTxs = confirmedTxs.sortWith((e1, e2) => e1.height < e2.height)
    historyEntry.copy(history = sortedConfirmedTxs ++ unconfirmedTxs)
  }
}
