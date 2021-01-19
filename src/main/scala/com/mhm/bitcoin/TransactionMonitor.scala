package com.mhm.bitcoin

import java.util.concurrent.ConcurrentHashMap

import com.mhm.common.model.{AddressHistory, HistoryElement, HistoryEntry}
import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.EpsmiDataOps
import com.mhm.util.EpsmiDataOps.{optAddr2Str, optConfirmations2Int, optSha2Str}
import com.mhm.util.HashOps.script2ScriptHash
import com.mhm.wallet.DeterministicWallet
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.core.protocol.transaction.CoinbaseInput
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

case class Tx4HistoryGen(confirmations: Int, txid: String, blockhashOpt: Option[DoubleSha256DigestBE])
case class LastKnown(lastKnownTx: Option[TxidAddress])
case class AddressBalance(confirmed: BigDecimal, unconfirmed: BigDecimal)


trait TransactionMonitor {
  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): TransactionMonitorState
  def checkForUpdatedTxs(state: TransactionMonitorState): (Set[String], TransactionMonitorState)
  def getAddressBalance(state: TransactionMonitorState, sh: String): Option[AddressBalance]
}


/**
 * @param rpcCli bitcoin core client
 * @param nonWalletAllowed if true allows extracting input transactions when they are not wallet transactions
 */
class TransactionMonitorImpl(rpcCli: BitcoindRpcExtendedClient, nonWalletAllowed: Boolean) extends TransactionMonitor with Logging {
  val ConfirmationsSafeFromReorg = 100
  val BATCH_SIZE = 1000

  def isTxHistoryEligible(tx: ListTransactionsResult, obtainedTxids: Set[String]): Boolean = {
    tx.txid.isDefined && Set("receive", "send", "generate", "immature").contains(tx.category) &&
    (tx.confirmations.isDefined && tx.confirmations.get >= 0) &&
    !obtainedTxids.contains(tx.txid.get.hex)
  }

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): TransactionMonitorState = {
    logger.trace("started buildAddressHistory")
    val ah = AddressHistory(
      monitoredScriptPubKeys.map(k => (script2ScriptHash(k), HistoryEntry(subscribed = false, Nil))).toMap
    )
    val state = TransactionMonitorState(addressHistory = ah, deterministicWallets = deterministicWallets)
    logger.trace(s"initialized address history keys with ${ah.m.keySet.size} key(s), head entry is ${ah.m.head}")

    val walletAddrScripthashes = ah.m.keySet

    @tailrec
    def insertTxsInHistory(state: TransactionMonitorState, transactions: List[ListTransactionsResult], obtainedTxids: Set[String]): (TransactionMonitorState, Set[String]) = transactions match {
      case Nil => (state, obtainedTxids)
      case tx::xs =>
        if (isTxHistoryEligible(tx, obtainedTxids)){
          logger.trace(s"tx ${tx.txid} deemed eligible based on category '${tx.category}' and more criteria")
          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val shToAddOut = walletAddrScripthashes.intersect(outputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAddIn = walletAddrScripthashes.intersect(inputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAdd = shToAddIn ++ shToAddOut
          logger.trace(s"${shToAdd.size} scripthashes to add")
          if (shToAdd.isEmpty) insertTxsInHistory(state, xs, obtainedTxids) else {
            for(wal <- deterministicWallets){
              val overrunDepths = wal.haveScriptpubkeysOverrunGaplimit(outputScriptpubkeys)
              if (overrunDepths.nonEmpty) throw new IllegalStateException(s"not enough addresses imported, overrun depths=$overrunDepths, see transactionmonitor.py line 155")
            }
            val tx4HistoryGen = Tx4HistoryGen(
              tx.confirmations.get,
              tx.txid.map(_.hex).get,
              tx.blockhash
            )
            val newHistoryElement = generateNewHistoryElement(tx4HistoryGen, txd)
            val state1 = state
              .addHistoryItemForScripthashes(shToAdd.toSeq, newHistoryElement)
            val state2 = if (tx.confirmations.isDefined && tx.confirmations.get > 0 && tx.confirmations.get < ConfirmationsSafeFromReorg){
                state1.addReorganizableTx(ReorganizableTxEntry(optSha2Str(tx.txid), tx.blockhash.map(_.hex), newHistoryElement.height, shToAdd.toSeq))
            } else state1
            insertTxsInHistory(state2, xs, obtainedTxids + optSha2Str(tx.txid))
          }
        }
        else insertTxsInHistory(state, xs, obtainedTxids)
    }

    @tailrec
    def processListTransactions(skip: Int, obtainedTxids: Set[String], state: TransactionMonitorState): TransactionMonitorState = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions")
      logger.trace(s"obtained ${transactions.size} transactions (skip=$skip) ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
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
    val state3 = state2.initUnconfirmedTxs()
    logger.trace(s"finished buildAddressHistory, history size = ${state3.addressHistory.m.size}, last known = ${state3.lastKnownTx.map(_.txid.substring(0,4))}")
    state3.addressHistory.m.foreach{ case (k, v) =>
      logger.trace(s"ah k=$k --> $v")
    }
    state3
  }

  def getInputAndOutputScriptpubkeys(txid: DoubleSha256DigestBE): (Seq[String], Seq[String], RpcTransaction) = {
    logger.trace(s"started getInputAndOutputScriptpubkeys for ${txid.hex}")
    val getTx = wrap(rpcCli.getTransaction(txid))
    val txd: RpcTransaction = wrap(rpcCli.decodeRawTransaction(getTx.hex))
    val outputScriptpubkeys = txd.vout.map(_.scriptPubKey.hex)
    logger.trace(s"got ${outputScriptpubkeys.size} output scriptpubkeys: ${outputScriptpubkeys.mkString("|")}")
    logger.trace(s"processing ${txd.vin.size} transaction inputs")
    val inputScriptpubkeys = txd.vin.filterNot(_.isInstanceOf[CoinbaseInput]).flatMap{ inn =>
      val inputTransactionId = DoubleSha256DigestBE.fromHex(inn.previousOutput.txIdBE.hex)
      logger.trace(s"processing input with transaction id: ${inputTransactionId.hex}")
      val resultTry = if (nonWalletAllowed)
        Try(wrap(rpcCli.getRawTransaction(inputTransactionId))).map(_.hex)
      else
        Try(wrap(rpcCli.getTransaction(inputTransactionId))).map(_.hex)
      resultTry match {
        case Failure(_) =>
          logger.trace(s"could not obtain transaction for input ${inputTransactionId.hex}")
          None
        case Success(inputTransaction) =>
          logger.trace(s"obtained transaction for input ${inputTransactionId.hex}")
          Some {
            logger.trace(s"decoding input transaction ${inputTransaction.hex}")
            val inputDecoded = wrap(rpcCli.decodeRawTransaction(inputTransaction))
            val script = inputDecoded.vout(inn.previousOutput.vout.toInt).scriptPubKey.hex
            logger.trace(s"obtained output script for input ${inputTransactionId.hex} which is: $script")
            script
          }
      }
    }
    logger.trace(s"got ${inputScriptpubkeys.size} input scriptpubkeys: ${inputScriptpubkeys.mkString("|")}")
    logger.trace(s"finished getInputAndOutputScriptpubkeys with ${outputScriptpubkeys.size} output(s) and ${inputScriptpubkeys.size} input(s)")
    (outputScriptpubkeys, inputScriptpubkeys, txd)
  }

  def generateNewHistoryElement(tx: Tx4HistoryGen, txd: RpcTransaction): HistoryElement = {
    case class ValueConfirmedPair(v: BigDecimal, confirmed: Boolean)
    if (tx.confirmations == 0){
      val valueConfirmedPairs = txd.vin.flatMap{ inn =>
        logger.trace(s"calling getTxOut with ${inn.previousOutput.txIdBE}, ${inn.previousOutput.vout.toInt}")
        /**
         * Note - for utxo that is spent we will get exception so we use try here
         */
        Try(wrap(rpcCli.getTxOut(inn.previousOutput.txIdBE, inn.previousOutput.vout.toInt))).fold(_ => None, utxo =>
          Some(ValueConfirmedPair(utxo.value.toBigDecimal, utxo.confirmations > 0))
        )
      }
      val totalInputValue = valueConfirmedPairs.map(_.v).fold(BigDecimal(0))(_ + _)
      val unconfirmedInput = !valueConfirmedPairs.map(_.confirmed).forall(_ == true)
      val height = if (unconfirmedInput) -1 else 0
      val totalOut = txd.vout.map(_.value.toBigDecimal).foldLeft(BigDecimal(0))(_ + _)
      val fee = totalInputValue - totalOut
      HistoryElement(tx.txid, height, fee)
    }
    else {
      val blockHeader = wrap(rpcCli.getBlockHeader(tx.blockhashOpt.getOrElse(throw new IllegalArgumentException("blockhash missing"))))
      HistoryElement(tx.txid, blockHeader.height)
    }
  }


  def checkForNewTxs(stateArg: TransactionMonitorState): TransactionMonitorState = {
    val maxAttempts = 8 // log base 2 of 256
    val state = stateArg.resetUpdatedScripthashes()

    case class GetTrRes(
      containsKnown: Boolean,
      newBegin: Int,
      newEnd: Int, // after end
      transactions: Vector[ListTransactionsResult]
    ){
      val knownBegin: Int = newEnd
      val knownEnd: Int = transactions.size // after end
      def isKnownOnly: Boolean = containsKnown && newEnd == 0
    }

    @tailrec
    def getTransactions(attempt: Int, maxAttempts: Int, count: Int, v: Vector[ListTransactionsResult], lastKnownTx: Option[TxidAddress]): GetTrRes = {
      if (attempt == maxAttempts) GetTrRes(containsKnown = false, 0, v.size, v)
      else {
        val transactions = wrap(rpcCli.listTransactions("*", count, 0, includeWatchOnly = true), "listTransactions").reverse
        logger.trace(s"obtained ${transactions.size} transactions (skip=0) ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
        lastKnownTx match {
          case None => GetTrRes(containsKnown = false, 0, transactions.size, transactions)
          case Some(ln) =>
            val found = transactions.zipWithIndex.find{ case (t, _) => optSha2Str(t.txid) == ln.txid && optAddr2Str(t.address) == ln.address }
            logger.trace(s"found last known ${ln.txid.substring(0,4)} at index: ${found.map(_._2)} from among: ${transactions.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
            found match {
              case Some((_, recentTxIndex)) if count != transactions.size => GetTrRes(containsKnown = true, 0, recentTxIndex, transactions)
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
        logger.debug(s"updating state for $txid")
        val blockhashOpt = tx.blockhash
        val (outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
        val matchingScripthashes = (outputScriptpubkeys ++ inputScriptpubkeys)
          .map(script2ScriptHash).filter(state.addressHistory.m.keySet.contains)
        val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(optConfirmations2Int(tx.confirmations), txid, blockhashOpt), txd)
        logger.trace(s"adding new history element: ${newHistoryElement.txHash} height=${newHistoryElement.height}")
        val isReorganizable = tx.confirmations.isDefined && tx.confirmations.get > 0 && tx.confirmations.get < ConfirmationsSafeFromReorg
        val isUnconfirmed = newHistoryElement.height <= 0
        logger.trace(s"adding with: reorganizable=$isReorganizable, unconfirmed=$isUnconfirmed")
        val newState = state
          .addUpdatedScripthashes(matchingScripthashes)
          .addHistoryItemForScripthashes(matchingScripthashes, newHistoryElement)

        val newState2 = if (isUnconfirmed) {
            logger.trace(s"adding unconfirmed for $txid")
            newState.addUnconfirmedScripthases(txid, matchingScripthashes)
        } else newState

        val newState3 = if (isReorganizable) {
            logger.trace(s"adding reorganizable for $txid")
            newState2.addReorganizableTx(ReorganizableTxEntry(txid, blockhashOpt.map(_.hex), newHistoryElement.height, matchingScripthashes))
        } else newState2

        updateStateWithTransactions(newState3, xs)
      }
    }
    val getTrRes = getTransactions(0, maxAttempts, 2, Vector(), state.lastKnownTx)
    val resultState = if (getTrRes.isKnownOnly){
      val newLastKnownTx = Some(getTrRes.transactions(getTrRes.knownBegin)).map { result => TxidAddress(optSha2Str(result.txid), optAddr2Str(result.address)) }
      val state1 = if (getTrRes.transactions.nonEmpty) state.setLastKnownTx(newLastKnownTx) else state
      state1
    } else {
      val newTxs = getTrRes.transactions.slice(getTrRes.newBegin, getTrRes.newEnd)
      logger.debug(s"new txs slice: ${newTxs.map(_.txid.map(_.hex.substring(0,4))).mkString("|")}")
      val relevantTxs = newTxs
        .filter(_.txid.isDefined)
        .filter(tx => Set("receive", "send", "generate", "immature").contains(tx.category))
        .filter(_.confirmations.getOrElse(0) >= 0)
      updateStateWithTransactions(state.setLastKnownTx(newTxs.headOption.map { result => TxidAddress(optSha2Str(result.txid), optAddr2Str(result.address)) }), relevantTxs.toList)
    }
    logger.debug(s"finished checkForNewTxs, found ${resultState.updatedScripthashes.size} new tx(s)")
    resultState.addressHistory.m.foreach {
      case (k, v) if v.history.nonEmpty => logger.debug(s"ah k=$k --> $v")
      case (k, v) => ()
    }
    resultState
  }

  def checkConfirmations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def processUnconfirmedTxs(state: TransactionMonitorState, unconfirmedTxs: List[UnconfirmedTxEntry]): TransactionMonitorState = unconfirmedTxs match {
      case Nil => state
      case UnconfirmedTxEntry(txid, shs) :: xs =>
        val unconfirmed = UnconfirmedTxEntry(txid, shs)
        val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
        logger.trace(s"uc_txid=$txid => $tx")
        if (tx.confirmations == 0){
          processUnconfirmedTxs(state, xs) // still unconfirmed
        }
        else if (tx.confirmations > 0){
          logger.info(s"Transaction confirmed: $txid")
          val block = wrap(rpcCli.getBlockHeader(EpsmiDataOps.optSha2Sha(tx.blockhash)))
          val blockHeight = block.height
          val newState = state
            .deleteHistoryItemForScripthashes(shs, txid)
            .addHistoryItemForScripthashes(shs, HistoryElement(txid, blockHeight, 0))
            .addReorganizableTx(ReorganizableTxEntry(txid, tx.blockhash.map(_.hex), blockHeight, shs))
            .removeUnconfirmed(Seq(unconfirmed))
            .addUpdatedScripthashes(shs)
          processUnconfirmedTxs(newState, xs)
        }
        else {
          logger.warn(s"Transaction conflicted: $txid")
          val newState = state
            .deleteHistoryItemForScripthashes(shs, txid)
            .addUpdatedScripthashes(shs)
          processUnconfirmedTxs(newState, xs)
        }
    }
    val newState = processUnconfirmedTxs(state, state.unconfirmedTxes.map{case(k, v) => UnconfirmedTxEntry(k, v)}.toList)
    logger.trace(s"finished checkConfirmations, unconfirmed=${newState.unconfirmedTxes.keys.mkString("|")}")
    val unconfirmedInAh = newState.addressHistory.m.collect{
      case (_,h) => h.history.filter(_.height <= 0)
    }.flatten.map(_.txHash)
    logger.trace(s"unconfirmed in ah: ${unconfirmedInAh.mkString("|")}")
    newState
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForReorganizations(state: TransactionMonitorState): TransactionMonitorState = {
    @tailrec
    def processReorganizableTxs(state: TransactionMonitorState, reorganizableTxs: List[ReorganizableTxEntry]): TransactionMonitorState = reorganizableTxs match {
      case Nil => state
      case ReorganizableTxEntry(txid, blockhash, height, matchingShs) :: xs =>
        val reorganizable = ReorganizableTxEntry(txid, blockhash, height, matchingShs)
        val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(txid)))
        if (tx.confirmations >= ConfirmationsSafeFromReorg){
          logger.debug(s"Transaction considered safe from reorg: $txid")
          processReorganizableTxs(state.removeReorganizable(reorganizable), xs)
        } else if (tx.confirmations < 1){
          val state2 = if (tx.confirmations == 0){
            // transaction became unconfirmed in reorg
            logger.info(s"Transaction was reorg'd out: $txid")
            val isOrphan = tx.details.head.category == "orphan"
            val state1 = state.addUnconfirmedScripthases(txid, matchingShs)
            val state2 = if (!isOrphan) { // TODO not sure why details is a vector and if this category extraction is correct here
                val txd = wrap(rpcCli.decodeRawTransaction(tx.hex))
                val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(tx.confirmations, tx.txid.hex, tx.blockhash), txd)
                state1.addHistoryItemForScripthashes(matchingShs, newHistoryElement)
              } else state1
            state2
          } else { // confirmations < 0
            logger.info(s"Transaction was double spent! $txid")
            state
          }
          processReorganizableTxs(state2.addUpdatedScripthashes(matchingShs).removeReorganizable(reorganizable).deleteHistoryItems(matchingShs, txid, height), xs)
        } else if (!tx.blockhash.map(_.hex).contains(blockhash)){
          val block = wrap(rpcCli.getBlockHeader(tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))))
          if (block.height == height) { //reorg but height is the same
            logger.debug(s"Transaction was reorg'd but still confirmed at same height: $txid")
            processReorganizableTxs(state, xs)
          }
          else {
            //reorged but still confirmed at a different height
            logger.debug("Transaction was reorg'd but still confirmed to a new block and different height: $txid")
            val state1 = state
              .addUpdatedScripthashes(matchingShs)
              .updateHeightForScripthashes(matchingShs, txid, block.height)
              .addReorganizableTx(reorganizable.copy(height = block.height))
              .removeReorganizable(reorganizable)
            processReorganizableTxs(state1, xs)
          }
        } else {
          processReorganizableTxs(state, xs)
        }
    }
    val newState = processReorganizableTxs(state, state.reorganizableTxes.toList)
    logger.trace("finished checkForReorganizations")
    newState
  }

  /**
   * @return set of updated scripthashes
   */
  def checkForUpdatedTxs(state: TransactionMonitorState): (Set[String], TransactionMonitorState) = {
    logger.debug("started checkForUpdatedTxs")
    val state1 = checkForNewTxs(state)
    val state2 = checkConfirmations(state1)
    val state3 = checkForReorganizations(state2)
    val updatedScripthashes = state3.sortAddressHistory().updatedScripthashes
    if (updatedScripthashes.nonEmpty) {
      logger.debug(s"unconfirmed txes = ${state3.unconfirmedTxes.keys.mkString("|")}")
      logger.debug(s"reorganizable_txes = ${state3.reorganizableTxes.mkString("|")}")
      logger.debug(s"updated_scripthashes = ${updatedScripthashes.mkString("|")}")
    }
    val updated = updatedScripthashes.filter(sh => state3.addressHistory.m.contains(sh) && state3.addressHistory.m(sh).subscribed)
    logger.debug(s"finished checkForUpdatedTxs, updated size = ${updated.size}")
    (updated.toSet, state3)
  }


  private def doGetAddressBalance(sh: String, history: Seq[HistoryElement]): Option[AddressBalance] = {
    case class AmountConfirmations(amount: BigDecimal, confirmations: Int)
    @tailrec
    def go(hes: List[HistoryElement], utxos: Map[String,AmountConfirmations]): Map[String,AmountConfirmations] = hes match {
      case Nil => utxos
      case h::xs =>
        val tx = wrap(rpcCli.getTransaction(DoubleSha256DigestBE.fromHex(h.txHash)))
        val txd = wrap(rpcCli.decodeRawTransaction(tx.hex))
        val utxosToAdd = txd.vout.zipWithIndex.collect {
          case (output, index) if script2ScriptHash(output.scriptPubKey.hex) == sh =>
            s"${txd.txid}:$index" -> AmountConfirmations(output.value.toBigDecimal, tx.confirmations)
        }.toMap
        val utxosToDel = txd.vin.map{ input => s"${input.previousOutput.txId}:${input.previousOutput.vout}" }
        go(xs, (utxos ++ utxosToAdd) -- utxosToDel)
    }
    val utxos = go(history.toList, Map())
    val confirmedBalance = utxos.values.filter(_.confirmations > 0).map(_.amount * BigDecimal(100000000)).sum
    val unconfirmedBalance = utxos.values.filter(_.confirmations == 0).map(_.amount * BigDecimal(100000000)).sum
    Some(AddressBalance(confirmedBalance, unconfirmedBalance))
  }

  def getAddressBalance(state: TransactionMonitorState, sh: String): Option[AddressBalance] = {
    val history = state.getElectrumHistory(sh)
    history match {
      case None => None
      case Some(Nil) => None
      case Some(history) => doGetAddressBalance(sh, history)
    }
  }
}
