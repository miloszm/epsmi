package com.mhm.api4electrum

import java.io.OutputStream
import java.util.concurrent.atomic.AtomicReference

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorState}
import grizzled.slf4j.Logging

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * This class conforms to json rpc requirements.
 * Uses Api4ElectrumCore for everything else.
 */

class Api4ElectrumImpl(core: Api4ElectrumCore, transactionMonitor: TransactionMonitor, monitorState: TransactionMonitorState) extends Api4Electrum with Logging {

  val currentMonitorState = new AtomicReference(monitorState)

  def updateMonitorState(fun : TransactionMonitorState => TransactionMonitorState): Unit = {
    def uniFun(s: TransactionMonitorState): TransactionMonitorState = fun(s)
    currentMonitorState.updateAndGet(uniFun)
  }

  def updateMonitorStateWithExtraResult[T](fun : TransactionMonitorState => (T, TransactionMonitorState)) : T = {
    var t : Option[T] = None
    def uniFun(s: TransactionMonitorState): TransactionMonitorState = {
      val (a, b) = fun(s)
      t = Some(a)
      b
    }
    currentMonitorState.updateAndGet(uniFun)
    t.get
  }

  override def serverVersion(v1: String, v2: String): Array[String] = {
    Array("epsmi 0.0.2")
  }
  override def blockchainBlockHeader(height: Int): String = {
    Try(Await.result(core.getBlockHeaderHash(height), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalArgumentException(s"height $height out of range")
      },
      identity
    )
  }

  override def blockchainBlockGetHeader(height: Int): HeaderResult = {
    Try(Await.result(core.getBlockHeaderX(height), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalArgumentException(s"height $height out of range")
      },
      a =>
        a
    )
  }

  override def estimateFee(waitBlocks: Int): BigDecimal = {
    Try(Await.result(core.estimateSmartFee(waitBlocks), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"estimate fee for $waitBlocks block(s) wait failed")
      },
      a =>
        a.setScale(8, RoundingMode.FLOOR)
    )
  }

  override def blockchainBlockGetChunk(index: Int): String = {
    Try(Await.result(core.getBlockChunk(index), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get block chunk for index $index failed")
      },
      identity
    )
  }

  override def blockchainBlockHeaders(startHeight: Int, count: Int): BlockHeadersResult = {
    Try(Await.result(core.getBlockHeaders(startHeight, count), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get block headers for start height $startHeight, count $count failed")
      },
      identity
    )
  }

  override def blockchainTransactionGet(txId: String): String = {
    Try(Await.result(core.getTransaction(txId: String), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get transaction for $txId failed")
      },
      identity
    )
  }

  override def blockchainTrIdFromPos(height: Int, txPos: Int, merkle: Boolean): String = {
    Try(Await.result(core.trIdFromPos(height, txPos, merkle), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get transaction id for $height, $txPos, merkle=$merkle failed")
      },
      identity
    )
  }

  override def blockchainTrIdFromPosMerkleTrue(height: Int, txPos: Int, merkle: Boolean): MerkleResult = ???

  override def blockchainTransactionGetMerkle(txId: String): GetMerkleResult = {
    Try(Await.result(core.transactionGetMerkle(txId: String), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"transaction get merkle for $txId failed")
      },
      identity
    )
  }

  override def blockchainScripthashSubcribe(sh: String): String = {
    val state = currentMonitorState.updateAndGet(_.subscribeAddress(sh))
    if (!state.addressHistory.m.contains(sh)){
      logger.warn("Address not known to server, hash(address)"
        + " = " + sh + ".\nCheck that you've imported the "
        + "master public key(s) correctly. The first three "
        + "addresses of each key are printed out on startup,\nso "
        + "check that they really are addresses you expect. In "
        + "Electrum go to Wallet -> Information to get the right "
        + "master public key.")
    }
    val historyHash = state.getElectrumHistoryHash(sh)
    logger.trace(s"historyHash for script hash $sh is${if (historyHash.isEmpty) " empty" else ": " + historyHash}")
    historyHash
  }

  def onUpdatedScripthashes(
    updatedScripthashes: Set[String],
    outputStream: OutputStream): Unit = {
    updatedScripthashes.foreach { sh =>
      val historyHash = currentMonitorState.get.getElectrumHistoryHash(sh)
      val update = s"""{"method": "blockchain.scripthash.subscribe", "params": [$sh, $historyHash]}"""
      outputStream.write(update.getBytes())
    }
  }

  def triggerHeartbeatConnected(outputStream: OutputStream): Unit = {
    val updatedTxs = updateMonitorStateWithExtraResult(transactionMonitor.checkForUpdatedTxs)
    onUpdatedScripthashes(updatedTxs, outputStream)
  }
}
