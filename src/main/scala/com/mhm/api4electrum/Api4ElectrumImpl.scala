package com.mhm.api4electrum

import java.io.OutputStream

import com.mhm.bitcoin.{TransactionMonitor, TransactionMonitorState}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * This class conforms to json rpc requirements.
 * Uses Api4ElectrumCore for everything else.
 */

class Api4ElectrumImpl(core: Api4ElectrumCore) extends Api4Electrum {
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
    Try(Await.result(core.getBlockHeader(height), Duration(20, SECONDS))).fold(
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

  def onUpdatedScripthashes(
    updatedScripthashes: Set[String],
    outputStream: OutputStream,
    transactionMonitor: TransactionMonitor,
    monitorState: TransactionMonitorState): Unit = {
//    updatedScripthashes.foreach { sh =>
//      val historyHash = transactionMonitor.getElectrumHistoryHash(sh, monitorState)
//      update = {"method": "blockchain.scripthash.subscribe", "params": [scrhash, history_hash]}
//      self._send_update(update)
//      send the update to outputStream
//    }
  }
}
