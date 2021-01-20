package com.mhm.api4electrum

import java.io.OutputStream
import java.util.concurrent.atomic.AtomicReference

import com.mhm.bitcoin.{
  NoopTxsMonitorStateListener,
  TransactionMonitor,
  TransactionMonitorState,
  TxsMonitorStateListener
}
import com.mhm.common.model.HexHeight
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.main.Constants
import com.mhm.main.Constants.{SERVER_NAME, SERVER_VERSION}
import grizzled.slf4j.Logging

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
  * This class conforms to json rpc requirements.
  * Uses Api4ElectrumCore for everything else.
  */
class Api4ElectrumImpl(core: Api4ElectrumCore,
                       transactionMonitor: TransactionMonitor,
                       monitorState: TransactionMonitorState,
                       monitorStateListener: TxsMonitorStateListener = NoopTxsMonitorStateListener)
    extends Api4Electrum
    with Logging {

  val currentMonitorState = new AtomicReference(monitorState)

  def updateMonitorState(fun: TransactionMonitorState => TransactionMonitorState): Unit = {
    def uniFun(s: TransactionMonitorState): TransactionMonitorState = fun(s)
    monitorStateListener.updated(currentMonitorState.updateAndGet(uniFun))
  }

  def updateMonitorStateWithExtraResult[T](fun: TransactionMonitorState => (T, TransactionMonitorState)): T = {
    var t: Option[T] = None
    def uniFun(s: TransactionMonitorState): TransactionMonitorState = {
      val (a, b) = fun(s)
      t = Some(a)
      b
    }
    monitorStateListener.updated(currentMonitorState.updateAndGet(uniFun))
    t.get
  }

  override def serverVersion(v1: String, v2: String): Array[String] = {
    Array(SERVER_NAME, SERVER_VERSION)
  }
  override def blockchainBlockHeader(height: Int): String = {
    Try(wrap(core.getBlockHeaderHash(height))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalArgumentException(s"height $height out of range")
    }, identity)
  }

  override def blockchainBlockGetHeader(height: Int): HeaderResult = {
    Try(wrap(core.getBlockHeader(height))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalArgumentException(s"height $height out of range")
    }, a => a)
  }

  override def estimateFee(waitBlocks: Int): BigDecimal = {
    Try(wrap(core.estimateSmartFee(waitBlocks))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalStateException(s"estimate fee for $waitBlocks block(s) wait failed")
    }, a => a.setScale(8, RoundingMode.FLOOR))
  }

  override def blockchainBlockGetChunk(index: Int): String = {
    Try(wrap(core.getBlockChunk(index))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalStateException(s"get block chunk for index $index failed")
    }, identity)
  }

  override def blockchainBlockHeaders(startHeight: Int, count: Int): BlockHeadersResult = {
    Try(wrap(core.getBlockHeaders(startHeight, count))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalStateException(s"get block headers for start height $startHeight, count $count failed")
    }, identity)
  }

  override def blockchainTransactionGet(txId: String): String = {
    Try(wrap(core.getTransaction(txId: String))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalStateException(s"get transaction for $txId failed")
    }, identity)
  }

  override def blockchainTrIdFromPos(height: Int, txPos: Int, merkle: Boolean): String = {
    Try(wrap(core.trIdFromPos(height, txPos, merkle))).fold({ t =>
      println(s"server caught: $t")
      throw new IllegalStateException(s"get transaction id for $height, $txPos, merkle=$merkle failed")
    }, identity)
  }

  override def blockchainTrIdFromPosMerkleTrue(height: Int, txPos: Int, merkle: Boolean): MerkleResult =
    ???

  override def blockchainTransactionGetMerkle(txId: String, height: Int): GetMerkleResult = {
    Try(wrap(core.transactionGetMerkle(txId: String))).fold({ t =>
      logger.error(s"server caught: $t")
      throw t
    }, identity)
  }

  override def blockchainScripthashSubcribe(sh: String): String = {
    val state = currentMonitorState.updateAndGet(_.subscribeAddress(sh))
    if (!state.addressHistory.m.contains(sh)) {
      logger.warn(
        "Address not known to server, hash(address)"
          + " = " + sh + ".\nCheck that you've imported the "
          + "master public key(s) correctly. The first three "
          + "addresses of each key are printed out on startup,\nso "
          + "check that they really are addresses you expect. In "
          + "Electrum go to Wallet -> Information to get the right "
          + "master public key."
      )
    }
    val historyHash = state.getElectrumHistoryHash(sh)
    logger.trace(s"historyHash for script hash $sh is${if (historyHash.isEmpty) " empty"
    else ": " + historyHash}")
    if (historyHash.isEmpty) null else historyHash
  }

  override def blockchainHeadersSubcribe(): HeadersSubscribeResult = {
    updateMonitorState(_.copy(subscribedToHeaders = true))
    val (_, hexHeight) = wrap(core.getCurrentHeaderRaw())
    HeadersSubscribeResult(hex = hexHeight.hex, height = hexHeight.height)
  }

  override def blockchainScripthashGetHistory(sh: String): Array[HistoryItem] = {
    val history =
      currentMonitorState.get.getElectrumHistory(sh).getOrElse(Nil).map { e =>
        HistoryItem(height = e.height, tx_hash = e.txHash, fee = Math.max(0, e.fee.toInt))
      }
    if (history.isEmpty) {
      logger.warn(s"Address history not known to server, hash(address) = $sh")

      /**
        * NOTE original eps skips sending response at all
        */
      Array()
    } else {
      history.toArray
    }
  }

  override def serverPing(): Unit = {}

  override def blockchainScripthashGetBalance(sh: String): GetBalanceResult = {
    transactionMonitor.getAddressBalance(currentMonitorState.get, sh) match {
      case None => throw new IllegalArgumentException(s"script hash not known")
      case Some(balance) =>
        GetBalanceResult(balance.confirmed, balance.unconfirmed)
    }
  }

  override def serverPeersSubscribe(): Array[String] = Array.empty[String]

  override def serverDonationAddress(): String = Constants.DONATION_ADDRESS

  override def mempoolGetFeeHistogram(): Array[Array[Int]] = {
    core.mempoolGetFeeHistogram()
  }

  override def blockchainRelayFee(): BigDecimal = {
    core.relayFee()
  }

  override def serverBanner(): String = {
    core.serverBanner(monitorState)
  }

  override def blockchainTransactionBroadcast(txhex: String): String = {
    core.blockchainTransactionBroadcast(txhex)
  }

  def onUpdatedScripthashes(updatedScripthashes: Set[String], outputStream: OutputStream): Unit = {
    updatedScripthashes.foreach { sh =>
      val historyHash = currentMonitorState.get.getElectrumHistoryHash(sh)
      val update      = s"""{"jsonrpc": "2.0", "method": "blockchain.scripthash.subscribe", "params": ["$sh", "$historyHash"]}""" + "\n"
      logger.info(s"writing update $update for sh: $sh with history hash: $historyHash")
      outputStream.write(update.getBytes())
    }
  }

  def onBlockchainTipUpdated(hexHeight: HexHeight, outputStream: OutputStream): Unit = {
    logger.debug(s"onBlockchainTipUpdated, subscribed to headers=${currentMonitorState.get.subscribedToHeaders}")
    if (currentMonitorState.get.subscribedToHeaders) {
      val update = s"""{"jsonrpc": "2.0", "method": "blockchain.headers.subscribe", "params": [{"hex": "${hexHeight.hex}", "height": ${hexHeight.height}}]}""" + "\n"
      outputStream.write(update.getBytes())
    }
  }

  def triggerHeartbeatConnected(outputStream: OutputStream): Unit =
    try {
      logger.debug("start triggerHeartbeatConnected")
      val (isTipUpdated, tipHexHeight) = wrap(core.checkForNewBlockchainTip())
      if (isTipUpdated) {
        logger.debug(s"Blockchain tip updated ${tipHexHeight.height}")
        onBlockchainTipUpdated(tipHexHeight, outputStream)
      }
      val updatedShs = updateMonitorStateWithExtraResult(transactionMonitor.checkForUpdatedTxs)
      monitorStateListener.updatedShsTick(updatedShs.toSeq)
      onUpdatedScripthashes(updatedShs, outputStream)
    } catch {
      case e: java.util.concurrent.TimeoutException =>
        logger.warn(
          s"timeout when processing heartbeat connected, caught: ${e.getClass.getCanonicalName} - ${e.getMessage}"
        )
      case e: Throwable =>
        logger.error(s"exception caught while servicing heartbeat connected: ${e.getClass.getCanonicalName}", e)
        throw e
    } finally {
      monitorStateListener.heartbeatTick()
      logger.debug("finished triggerHeartbeatConnected")
    }
}
