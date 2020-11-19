package com.mhm.bitcoin

import java.util.concurrent.ConcurrentHashMap

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.EpsmiDataOps.{optAddr2Str, optConfirmations2Int, optSha2Sha, optSha2Str}
import com.mhm.util.HashOps.script2ScriptHash
import com.mhm.util.{EpsmiDataOps, HashOps}
import com.mhm.wallet.DeterministicWallet
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.util.{Failure, Success, Try}

case class Tx4HistoryGen(confirmations: Int, txid: String, blockhash: DoubleSha256DigestBE)
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

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): TransactionMonitorState = {
    logger.debug("started buildAddressHistory")
    val ah = AddressHistory(
      monitoredScriptPubKeys.map(k => (script2ScriptHash(k), HistoryEntry(subscribed = false, Nil))).toMap
    )
    val state = TransactionMonitorState(ah)
    logger.trace(s"initialized address history keys with ${ah.m.keySet.size} keys, head entry is ${ah.m.head}")

    val walletAddrScripthashes = ah.m.keySet

    @tailrec
    def insertTxsInHistory(state: TransactionMonitorState, transactions: List[ListTransactionsResult], obtainedTxids: Set[String]): (TransactionMonitorState, Set[String]) = transactions match {
      case Nil => (state, obtainedTxids)
      case tx::xs =>
        if (isTxHistoryEligible(tx, obtainedTxids)){
          logger.trace(s"tx ${tx.txid} checked for category: '${tx.category}', confirmations '${tx.confirmations}' and more")
          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val shToAddOut = walletAddrScripthashes.intersect(outputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAddIn = walletAddrScripthashes.intersect(inputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAdd = shToAddIn ++ shToAddOut
          logger.trace(s"${shToAdd.size} scripthashes to add")
          if (shToAdd.isEmpty) insertTxsInHistory(state, xs, obtainedTxids) else {
            for(wal <- deterministicWallets){
              val overrunDepths = wal.haveScriptpubkeysOverrunGaplimit(outputScriptpubkeys)
              if (overrunDepths.nonEmpty) throw new IllegalStateException("not enough addresses imported, see transactionmonitor.py line 155")
            }
            val tx4HistoryGen = Tx4HistoryGen(tx.confirmations.get, tx.txid.map(_.hex).get, tx.blockhash.get)
            val newHistoryElement = generateNewHistoryElement(tx4HistoryGen, txd)
            val state1 = state.addHistoryItemForScripthashes(shToAdd.toSeq, newHistoryElement)
            insertTxsInHistory(state1, xs, obtainedTxids + optSha2Str(tx.txid))
          }
        }
        else insertTxsInHistory(state, xs, obtainedTxids)
    }

    @tailrec
    def processListTransactions(skip: Int, obtainedTxids: Set[String], state: TransactionMonitorState): TransactionMonitorState = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions")
      logger.debug(s"obtained ${transactions.size} transactions (skip=$skip) ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
      val lastTx = if (transactions.nonEmpty) transactions.headOption else None
      val state2 = state.setLastKnownTx(lastTx.map(last =>TxidAddress(optSha2Str(last.txid), optAddr2Str(last.address))))
      val (state3,newTxids) = insertTxsInHistory(state2, transactions.toList, Set())

      //val overrunDepths = deterministicWallets.map(_.)

      if (transactions.size == BATCH_SIZE)
        processListTransactions(skip + BATCH_SIZE, obtainedTxids ++ newTxids, state3)
      else
        state3
    }

    val state2 = processListTransactions(skip = 0, obtainedTxids = Set(), state).sortAddressHistory()
    logger.debug(s"finished buildAddressHistory, history size = ${state2.addressHistory.m.size}, last known = ${state2.lastKnownTx.map(_.txid.substring(0,4))}")
    state2
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


  /**
   * @return set of updated scripthashes
   */
  def checkForNewTxs(stateArg: TransactionMonitorState): TransactionMonitorState = {
    logger.debug("started checkForNewTxs")
    val maxAttempts = 8 // log base 2 of 256
    val state = stateArg.resetUpdatedScripthashes()

    @tailrec
    def getTransactions(attempt: Int, maxAttempts: Int, count: Int, v: Vector[ListTransactionsResult], lastKnownTx: Option[TxidAddress]): (Int, Vector[ListTransactionsResult]) = {
      if (attempt == maxAttempts) (0, v)
      else {
        val transactions = wrap(rpcCli.listTransactions("*", count, 0, includeWatchOnly = true), "listTransactions")
        logger.debug(s"obtained ${transactions.size} transactions (skip=0) ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
        lastKnownTx match {
          case None => (transactions.size-1, transactions)
          case Some(ln) =>
            val found = transactions.zipWithIndex.find{ case (t, _) => optSha2Str(t.txid) == ln.txid && optAddr2Str(t.address) == ln.address }
            logger.debug(s"found last known at index: ${found.map(_._2)} from among: ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
            found match {
              case Some((_, recentTxIndex)) if count != transactions.size => (recentTxIndex, transactions)
              case _ => getTransactions(attempt+1, maxAttempts, count * 2, transactions, lastKnownTx)
            }
        }
      }
    }
    @tailrec
    def updateStateWithTransactions(state: TransactionMonitorState, txs: List[ListTransactionsResult]): TransactionMonitorState = { txs match {
      case Nil => state
      case tx :: xs =>
        val txid = optSha2Str(tx.txid)
        val blockhash = optSha2Sha(tx.blockhash)
        val (outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
        val matchingScripthashes = (outputScriptpubkeys ++ inputScriptpubkeys)
          .map(script2ScriptHash).filter(state.addressHistory.m.keySet.contains)
        logger.debug(s"new tx: ${tx.txid.map(_.hex.substring(0,4))}")
        val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(optConfirmations2Int(tx.confirmations), txid, blockhash), txd)
        val newState = state
          .addUpdatedScripthashes(matchingScripthashes)
          .addHistoryItemForScripthashes(matchingScripthashes, newHistoryElement)
          .applyIf(newHistoryElement.height <= 0){
            _.addUnconfirmedScripthases(txid, matchingScripthashes)
          }
          .applyIf(tx.confirmations.getOrElse(-1) > 0) {
            _.addReorganizableTx(ReorganizableTxEntry(txid, blockhash.hex, newHistoryElement.height, matchingScripthashes))
          }
        updateStateWithTransactions(newState, xs)
      }
    }
    val (recentTxIndex, transactions) = getTransactions(0, maxAttempts, 2, Vector(), state.lastKnownTx)
    val newLastKnownTx = Some(transactions(recentTxIndex)).map { result => TxidAddress(optSha2Str(result.txid), optAddr2Str(result.address)) }
    val state1 = state.applyIf(transactions.nonEmpty){_.setLastKnownTx(newLastKnownTx)}
    val resultState = if (recentTxIndex == 0){
      state1
    } else {
      val newTxs = transactions.slice(0, recentTxIndex)
      logger.debug(s"new txs slice: ${newTxs.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
      val relevantTxs = newTxs
        .filter(_.txid.isDefined)
        .filter(tx => Set("receive", "send", "generate", "immature").contains(tx.category))
        .filter(_.confirmations.getOrElse(0) >= 0)
      updateStateWithTransactions(state1.setLastKnownTx(newTxs.headOption.map { result => TxidAddress(optSha2Str(result.txid), optAddr2Str(result.address)) }), relevantTxs.toList)
    }
    logger.debug(s"finished checkForNewTxs, found ${resultState.updatedScripthashes.size} new tx(s)")
    resultState
  }

  def checkForConfirmations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def go(state: TransactionMonitorState, unconfirmedTxs: List[UnconfirmedTxEntry]): TransactionMonitorState = unconfirmedTxs match {
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
    go(state, state.unconfirmedTxes.map{case(k, v) => UnconfirmedTxEntry(k, v)}.toList)
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForReorganizations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def go(state: TransactionMonitorState, reorganizableTxs: List[ReorganizableTxEntry]): TransactionMonitorState = reorganizableTxs match {
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
    go(state, state.reorganizableTxes.toList)
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForUpdatedTxs(state: TransactionMonitorState): (Set[String], TransactionMonitorState) = {
    logger.debug("started checkForUpdatedTxs")
    val state1 = checkForNewTxs(state).combineUpdatedScripthashes(checkForConfirmations(state))
      .combineUpdatedScripthashes(checkForReorganizations(state))
    val updatedScripthashes = state1.sortAddressHistory().updatedScripthashes
    if (updatedScripthashes.nonEmpty) {
      logger.debug(s"unconfirmed txes = ${unconfirmedTxes.keySet().asScala.mkString("|")}")
      logger.debug(s"reorganizable_txes = ${reorganizableTxes.mkString("|")}")
      logger.debug(s"updated_scripthashes = ${updatedScripthashes.mkString("|")}")
    }
    val updated = updatedScripthashes.filter(sh => state1.addressHistory.m.contains(sh) && state1.addressHistory.m(sh).subscribed)
    logger.debug(s"finished checkForUpdatedTxs, updated size = ${updated.size}")
    (updated.toSet, state1)
  }

}
