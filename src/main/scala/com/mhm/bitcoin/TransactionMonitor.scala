package com.mhm.bitcoin

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicReference

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.HashOps
import com.mhm.util.HashOps.script2ScriptHash
import com.mhm.wallet.DeterministicWallet
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, ListTransactionsResult, RpcTransaction}
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.Predef.Set
import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.{Failure, Success, Try}

case class Tx4HistoryGen(confirmations: Int, txid: String, blockhash: DoubleSha256DigestBE)

/**
 * @param rpcCli bitcoin core client
 * @param nonWalletAllowed if true allows extracting input transactions when they are not wallet transactions
 */
class TransactionMonitor(rpcCli: BitcoindRpcExtendedClient, nonWalletAllowed: Boolean) extends Logging {

  val ConfirmationsSafeFromReorg = 100

  case class TxidAddress(txid: String, address: String)

  case class LastKnown(lastKnownTx: Option[TxidAddress])

  val lastKnown = new AtomicReference[LastKnown](LastKnown(None))

  val unconfirmedTxes: ConcurrentHashMap[String,Seq[String]] = new java.util.concurrent.ConcurrentHashMap[String, Seq[String]]()

  val reorganizableTxes = ListBuffer[(String, String, Int, Seq[String])]()

  def isTxHistoryEligible(tx: ListTransactionsResult, obtainedTxids: Set[String]): Boolean = {
    tx.txid.isDefined && Set("receive", "send", "generate", "immature").contains(tx.category) &&
    (tx.confirmations.isDefined && tx.confirmations.get >= 0) &&
    !obtainedTxids.contains(tx.txid.get.hex)
  }

  def buildAddressHistory(
    monitoredScriptPubKeys: Seq[String],
    deterministicWallets: Seq[DeterministicWallet]
  ): AddressHistory = {
    logger.info("started buildAddressHistory")
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from[String, HistoryEntry](monitoredScriptPubKeys.map(k => (script2ScriptHash(k), HistoryEntry(false, Nil))))
    )
    logger.info(s"initialized address history keys with ${ah.m.keySet.size} keys, head entry is ${ah.m.head}")

    val BATCH_SIZE = 1000

    val walletAddrScripthashes = ah.m.keySet

    def go(skip: Int, obtainedTxids: Set[String]): Unit = {
      val transactions = wrap(rpcCli.listTransactions("*", BATCH_SIZE, skip, includeWatchOnly = true), "listTransactions")
      logger.info(s"obtained ${transactions.size} transactions (skip=$skip)")
      val lastTx = if ((transactions.size < BATCH_SIZE) && skip == 0) Some(transactions.last) else None
      val newTxids = (transactions.flatMap{ tx: ListTransactionsResult =>
        if (isTxHistoryEligible(tx, obtainedTxids)){
          logger.debug(s"tx ${tx.txid} checked for category: '${tx.category}', confirmations '${tx.confirmations}' and more")
          val(outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val shToAddOut = walletAddrScripthashes.intersect(outputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAddIn = walletAddrScripthashes.intersect(inputScriptpubkeys.map(script2ScriptHash).toSet)
          val shToAdd = shToAddIn ++ shToAddOut
          logger.debug(s"${shToAdd.size} scripthashes to add")
          if (shToAdd.isEmpty) None else {
            for(wal <- deterministicWallets){
              val overrunDepths = wal.haveScriptpubkeysOverrunGaplimit(outputScriptpubkeys)
              if (overrunDepths.nonEmpty) throw new IllegalStateException("not enough addresses imported, see transactionmonitor.py line 155")
            }
            val tx4HistoryGen = Tx4HistoryGen(tx.confirmations.get, tx.txid.map(_.hex).get, tx.blockhash.get)
            val newHistoryElement = generateNewHistoryElement(tx4HistoryGen, txd)
            shToAdd.foreach{ scriptHash =>
              logger.debug(s"adding history element (${newHistoryElement.txHash}, ${newHistoryElement.height}) to $scriptHash")
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
      else ()
    }
    go(skip = 0, obtainedTxids = Set())

    logger.debug(s"finished buildAddressHistory, history size = ${ah.m.size}")
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
      val resultTry = if (nonWalletAllowed)
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

  /**
   * @return set of updated scripthashes
   */
  def checkForNewTxs(ah: AddressHistory): Set[String] = {
    val MaxTxRequestCount = 256
    val txRequestCount = 2
    val maxAttempts = 8 // log base 2 of 256

    @tailrec
    def go(attempt: Int, maxAttempts: Int, count: Int, v: Vector[ListTransactionsResult], lastKnownTx: Option[TxidAddress]): (Int, Vector[ListTransactionsResult]) = {
      if (attempt == maxAttempts)
        (0, v)
      else {
        val transactions = wrap(rpcCli.listTransactions("*", count, 0, includeWatchOnly = true), "listTransactions").reverse
        lastKnownTx match {
          case None =>
            (transactions.size, transactions)
          case Some(ln) =>
            val found = transactions.zipWithIndex.find{ case (t, _) => t.txid.map(_.hex).getOrElse(throw new IllegalArgumentException("missing txid")) == ln.txid && t.address.map(_.value).contains(ln.address) }
            found match {
              case Some((_, recentTxIndex)) => (recentTxIndex, transactions)
              case None => go(attempt+1, maxAttempts, count * 2, transactions, lastKnownTx)
            }
        }
      }
    }
    val (recentTxIndex, ret) = go(0, maxAttempts, 2, Vector(), lastKnown.get().lastKnownTx)
    if (ret.nonEmpty){
      lastKnown.set(LastKnown(Some(TxidAddress(
        ret.head.txid.map(_.hex).getOrElse(throw new IllegalArgumentException("missing txid")),
        ret.head.address.map(_.value).getOrElse(throw new IllegalArgumentException("missing address"))
      ))))
    }
    if(recentTxIndex == 0){
      Set()
    } else {
      val newTxs = ret.slice(0, recentTxIndex).reverse
      logger.debug(s"new transactions=${newTxs.map(_.txid).mkString("|")}")
      val relevantTxs = newTxs
        .filter(_.txid.isDefined)
        .filter(tx => Set("receive", "send", "generate", "immature").contains(tx.category))
        .filter(_.confirmations.getOrElse(0) >= 0)
      val updatedScripthashes = (for {
        tx <- relevantTxs
      } yield {
          val txid = tx.txid.map(_.hex).getOrElse(throw new IllegalArgumentException("missing txid"))
          val blockhash = tx.blockhash.getOrElse(throw new IllegalArgumentException("missing blockhash"))
          val (outputScriptpubkeys, inputScriptpubkeys, txd) = getInputAndOutputScriptpubkeys(tx.txid.get)
          val matchingScripthashes = (outputScriptpubkeys ++ inputScriptpubkeys)
            .map(HashOps.script2ScriptHash).filter(ah.m.keySet.contains)
          val newHistoryElement = generateNewHistoryElement(Tx4HistoryGen(
            tx.confirmations.getOrElse(throw new IllegalArgumentException("missing confirmations")),
            txid,
            blockhash
          ), txd)
          logger.info(s"Found new tx: $newHistoryElement")
          matchingScripthashes.foreach {ms =>
            ah.m.get(ms) match {
              case Some(he) => ah.m.put(ms, he.copy(history = he.history :+ newHistoryElement))
              case None => ah.m.put(ms, HistoryEntry(false, Seq(newHistoryElement)))
            }
            if (newHistoryElement.height <= 0){
              unconfirmedTxes.put(txid, unconfirmedTxes.getOrDefault(txid, Nil) :+ ms)
            }
          }
          if (tx.confirmations.getOrElse(-1) > 0){
            reorganizableTxes.addOne((txid, blockhash.hex, newHistoryElement.height, matchingScripthashes))
          }
          matchingScripthashes
      })
      updatedScripthashes.foldLeft(Set[String]())((a,b) => a ++ b.toSet)
    }
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

}
