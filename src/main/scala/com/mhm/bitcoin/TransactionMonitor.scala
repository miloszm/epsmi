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

  val unconfirmedTxes: ConcurrentHashMap[String,Seq[String]] = new java.util.concurrent.ConcurrentHashMap[String, Seq[String]]()

  val reorganizableTxes = ListBuffer[ReorganizableTxEntry]()

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

    val lastKnown = ret.lastOption.map{ last =>TxidAddress(optSha2Str(last.txid), EpsmiDataOps.optAddr2Str(last.address)) }

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
    newUnconfirmed: ArrayBuffer[UnconfirmedTxEntry] = ArrayBuffer(),
    newReorganizable: ArrayBuffer[ReorganizableTxEntry] = ArrayBuffer()
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
              r.newUnconfirmed.addOne(UnconfirmedTxEntry(txid, Seq(ms)))
          }
          if (tx.confirmations.getOrElse(-1) > 0)
            r.newReorganizable.addOne(ReorganizableTxEntry(txid, blockhash.hex, newHistoryElement.height, matchingScripthashes))
      }
      r
    }
    logger.debug(s"finished checkForNewTxs, found ${result.newFound.size} new tx(s)")
    result
  }

  def checkForConfirmations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def go(state: TransactionMonitorState, unconfirmedTxs: Seq[UnconfirmedTxEntry]): TransactionMonitorState = unconfirmedTxs match {
      case Nil => state
      case UnconfirmedTxEntry(txid, shs) :: xs =>
        val unconfirmed = UnconfirmedTxEntry(txid, shs)
        val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
        logger.debug(s"uc_txid=$txid => $tx")
        if (tx.confirmations < 0)
          logger.warn(s"Transaction conflicted: $txid")
        if (tx.confirmations > 0) {
          val block = wrap(rpcCli.getBlockHeader(EpsmiDataOps.optSha2Sha(tx.blockhash)))
          val blockHeight = block.height
          val newState = state
            .deleteHistoryItemForScripthashes(shs, txid)
            .addReorganizableTx(ReorganizableTxEntry(txid, optSha2Str(tx.blockhash), blockHeight, shs))
            .removeUnconfirmed(Seq(unconfirmed))
            .addUpdatedScripthashes(shs)
          go(newState, xs)
        } else {
          go(state.removeUnconfirmed(Seq(unconfirmed)), xs)
        }
    }
    go(state, state.unconfirmedTxes.map{case(k, v) => UnconfirmedTxEntry(k, v)}.toSeq)
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForReorganizations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def go(state: TransactionMonitorState, reorganizableTxs: Seq[ReorganizableTxEntry]): TransactionMonitorState = reorganizableTxs match {
      case Nil => state
      case ReorganizableTxEntry(txid, blockhash, height, matchingShs) :: xs =>
        val reorganizable = ReorganizableTxEntry(txid, blockhash, height, matchingShs)
        val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
        if (tx.confirmations >= ConfirmationsSafeFromReorg){
          logger.debug(s"Transaction considered safe from reorg: $txid")
          go(state.removeReorganizable(reorganizable), xs)
        } else if (tx.confirmations < 1){
          val state2 = if (tx.confirmations == 0){
            // transaction became unconfirmed in reorg
            logger.info(s"Transaction was reorg'd out: $txid")
            val state2 = state.addUnconfirmedScripthases(txid, matchingShs)
            if (tx.details.head.category != "orphan") { // TODO not sure why details is a vector and if this category extraction is correct here
              val txd = wrap(rpcCli.decodeRawTransaction(tx.hex))
              val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(tx.confirmations, tx.txid.hex, EpsmiDataOps.optSha2Sha(tx.blockhash)), txd)
              state2.addHistoryItemForScripthashes(matchingShs, newHistoryElement)
            } else {
              state2
            }
          } else { // confirmations < 0
            logger.info(s"Transaction was double spent! $txid")
            state
          }
          go(state2.addUpdatedScripthashes(matchingShs).removeReorganizable(reorganizable), xs)
        } else if (!tx.blockhash.map(_.hex).contains(blockhash)){
          val block = wrap(rpcCli.getBlockHeader(tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))))
          if (block.height == height) { //reorg but height is the same
            logger.debug(s"Transaction was reorg'd but still confirmed at same height: $txid")
            go(state, xs)
          }
          else {
            //reorged but still confirmed at a different height
            logger.debug("Transaction was reorg'd but still confirmed to a new block and different height: $txid")
            val state1 = state
              .addUpdatedScripthashes(matchingShs)
              .updateHeightForScripthashes(matchingShs, txid, block.height)
              .addReorganizableTx(reorganizable.copy(height = block.height))
              .removeReorganizable(reorganizable)
            go(state1, xs)
          }
        } else {
          go(state, xs)
        }
    }
    go(state, state.reorganizableTxes)
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
