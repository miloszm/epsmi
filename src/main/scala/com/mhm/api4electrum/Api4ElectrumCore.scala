package com.mhm.api4electrum

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import com.fasterxml.jackson.databind.ObjectMapper
import com.mhm.bitcoin.TransactionMonitorState
import com.mhm.common.model.{HexHeight, OwnNode}
import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.main.Constants
import com.mhm.main.Constants.{DONATION_ADDRESS, SERVER_VERSION}
import com.mhm.util.EpsmiDataOps.{byteVectorOrZeroToArray, byteVectorToArray, intToArray, uint32ToArray}
import com.mhm.util.{HashOps, MerkleProofOps}
import grizzled.slf4j.Logging
import javax.xml.bind.DatatypeConverter
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.FeeEstimationMode
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, GetMemPoolResult}
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode
import scala.util.{Failure, Success, Try}



/**
 * This class is futurized and does not necessarily conform
 * to json rpc requirements.
 */

case class Api4ElectrumCore(
  rpcCli: BitcoindRpcExtendedClient,
  config: Api4ElectrumCoreConfig = Api4ElectrumCoreConfig.getDefault
)(implicit ec: ExecutionContext) extends Logging {
  val bestBlockHash = new AtomicReference[Option[String]](None)
  val printedSlowMempoolWarning = new AtomicBoolean(false)

  def getBlockHeaderHash(blockHeight: Int): Future[String] = {
    for {
      blockHash <- rpcCli.getBlockHash(blockHeight)
      (blockHeaderHash, _) <- getBlockHeaderHashFromBlockHash(blockHash)
    } yield {
      blockHeaderHash
    }
  }

  /**
   * returns block header hash and next block hash
   */
  def getBlockHeaderHashFromBlockHash(blockHash: DoubleSha256DigestBE): Future[(String,Option[DoubleSha256DigestBE])] = {
    for {
      blockHeader <- rpcCli.getBlockHeader(blockHash)
    } yield {
      val headHex = hexBlockHeaderRaw(blockHeader)
      (headHex, blockHeader.nextblockhash)
    }
  }

  private def hexBlockHeaderRaw(blockHeader: GetBlockHeaderResult): String = {
    // <i32s32sIII
    // little endian int | byte[32] | byte[32] | unsigned int | unsigned int | unsigned int
    val head = ByteBuffer.allocate(80)
    head.put(intToArray(blockHeader.version))
    head.put(byteVectorOrZeroToArray(blockHeader.previousblockhash.map(_.bytes), 32))
    head.put(byteVectorToArray(blockHeader.merkleroot.bytes))
    head.put(uint32ToArray(blockHeader.time))
    head.put(uint32ToArray(blockHeader.bits))
    head.put(uint32ToArray(blockHeader.nonce))

    val headHex = DatatypeConverter.printHexBinary(head.array())
    headHex.toLowerCase
  }

  def getBlockHeader(blockHeight: Int): Future[HeaderResult] = {
    for {
      blockHash <- rpcCli.getBlockHash(blockHeight)
      blockHeader <- rpcCli.getBlockHeader(blockHash)
    } yield {
      HeaderResult(
        blockHeader.height,
        blockHeader.previousblockhash.map(_.hex).getOrElse("00"*32),
        blockHeader.time.toLong,
        blockHeader.merkleroot.hex,
        blockHeader.version,
        blockHeader.nonce.toLong,
        blockHeader.bits.toLong
      )
    }
  }

  def getBlockHeaderCooked(blockhash: DoubleSha256DigestBE): Future[HeaderResult] = {
    for {
      blockHeader <- rpcCli.getBlockHeader(blockhash)
    } yield {
      val headerResult = HeaderResult(
        blockHeader.height,
        blockHeader.previousblockhash.map(_.hex).getOrElse("00" * 32),
        blockHeader.time.toLong,
        blockHeader.merkleroot.hex,
        blockHeader.version,
        blockHeader.nonce.toLong,
        blockHeader.bits.toLong
      )
      headerResult
    }
  }

  def getBlockHeaderRaw(blockhash: DoubleSha256DigestBE): Future[HexHeight] = {
    for {
      blockHeader <- rpcCli.getBlockHeader(blockhash)
    } yield {
      val headHex = hexBlockHeaderRaw(blockHeader)
      HexHeight(headHex, blockHeader.height)
    }
  }

  def estimateSmartFee(waitBlocks: Int): Future[BigDecimal] = {
    for {
      smartFeeResult <- rpcCli.estimateSmartFee(waitBlocks, FeeEstimationMode.Conservative)
    } yield {
      val fr = smartFeeResult.feerate match {
        case Some(fee) => BigDecimal(fee.toLong) / BigDecimal(100000)
        case _ => BigDecimal(0.0001)
      }
      fr.setScale(8, RoundingMode.FLOOR)
    }
  }

  private def doGetBlockHeaders(startHeight: Int, count: Int): Future[(String, Int)] = {
    @tailrec
    def go(blockHashOpt: Option[DoubleSha256DigestBE], headerHashes: List[String], count: Int): (List[String], Int) = {
      if (count > 0 && blockHashOpt.isDefined) {
        val (headerHash, nextBlockHashOpt) = wrap(getBlockHeaderHashFromBlockHash(blockHashOpt.get))
        go(nextBlockHashOpt, headerHash +: headerHashes, count - 1)
      } else {
        (headerHashes, count)
      }
    }
    if (count <= 0){
      Future.successful(("", 0))
    } else {
      Try(wrap(rpcCli.getBlockHash(startHeight))) match {
        case Success(firstBlockHash) =>
          val (headerHashes, restCount) = go(Some(firstBlockHash), Nil, count)
          Future.successful((headerHashes.reverse.mkString, count-restCount))
        case Failure(_) =>
          Future.successful(("", 0))
      }
    }
  }

  def getBlockChunk(index: Int): Future[String] = {
    val RETARGET_INTERVAL = 2016
    for {
      blockchainInfoResult <- rpcCli.getBlockChainInfo
      tipHeight = blockchainInfoResult.headers
      nextHeight = tipHeight + 1
      startHeight = Math.min(index*RETARGET_INTERVAL, nextHeight)
      count = Math.min(nextHeight - startHeight, RETARGET_INTERVAL)
      (headersHex,_) <- doGetBlockHeaders(startHeight, count)
    } yield {
      headersHex
    }
  }

  def getBlockHeaders(startHeight: Int, count: Int): Future[BlockHeadersResult] = {
    val MAX_CHUNK_SIZE = 2016
    val minCount = Math.min(count, MAX_CHUNK_SIZE)
    for {
      (headersHex, effectiveCount) <- doGetBlockHeaders(startHeight, minCount)
    } yield {
      BlockHeadersResult(headersHex, effectiveCount, MAX_CHUNK_SIZE)
    }
  }

  def getTransaction(txId: String): Future[String] = {
    val sha = DoubleSha256DigestBE.fromHex(txId.toUpperCase)
    rpcCli.getTransaction(sha).map(_.hex.hex).recoverWith{ _ =>
      rpcCli.getRawTransaction(sha).map(_.hex.hex)
    }
  }

  def trIdFromPos(height: Int, txPos: Int, merkle: Boolean): Future[String] = {
    if (merkle)
      trIdFromPosMerkleTrue(height, txPos)
    else
      trIdFromPosMerkleFalse(height, txPos)
  }

  private def trIdFromPosMerkleFalse(height: Int, txPos: Int): Future[String] = {
    for {
      blockHash <- rpcCli.getBlockHash(height)
      block <- rpcCli.getBlock(blockHash)
    } yield {
        val txId = block.tx(txPos)
        txId.hex
    }
  }

  private def trIdFromPosMerkleTrue(height: Int, txPos: Int): Future[String] = {
    val merkleResultFuture = for {
      blockHash <- rpcCli.getBlockHash(height)
      block <- rpcCli.getBlock(blockHash)
      txId = block.tx(txPos)
      merkleBlock <- rpcCli.getTxOutProof(Vector(txId), blockHash)
    } yield {
      val emp = MerkleProofOps.convertCoreToElectrumMerkleProof(merkleBlock.hex)
      MerkleResult(txId.hex, emp.merkle)
    }
    merkleResultFuture.map{mr =>
      val objectMapper = new ObjectMapper()
      val out = new ByteArrayOutputStream()
      objectMapper.writeValue(out, mr)
      out.toString
    }
  }

  def transactionGetMerkle(txId: String): Future[GetMerkleResult] = {
    val sha = DoubleSha256DigestBE.fromHex(txId.toUpperCase)
    for {
      transactionResult <- rpcCli.getRawTransaction(sha)
      blockHeader <- rpcCli.getBlockHeader(transactionResult.blockhash.getOrElse(throw new IllegalStateException(s"blockhash missing for $txId")))
      coreProof <- rpcCli.getTxOutProof(Vector(sha), transactionResult.blockhash.getOrElse(throw new IllegalStateException(s"blockhash missing for $txId")))
    } yield {
      val electrumProof = MerkleProofOps.convertCoreToElectrumMerkleProof(coreProof.hex)
      val impliedMerkleRoot = HashOps.hashMerkleRoot(electrumProof.merkle, txId, electrumProof.pos)
      if (impliedMerkleRoot != electrumProof.merkleRoot)
        throw new IllegalStateException(s"value error in get merkle for $txId, implied merkle root: $impliedMerkleRoot is not equal Electrum merkle root: ${electrumProof.merkleRoot}")
      GetMerkleResult(blockHeader.height, electrumProof.pos, electrumProof.merkle)
    }
  }

  def getCurrentHeaderCooked(): Future[(String, HeaderResult)] = {
    for {
      bestBlockHash <- rpcCli.getBestBlockHash
      header <- getBlockHeaderCooked(bestBlockHash)
    } yield {
      (bestBlockHash.hex, header)
    }
  }

  def getCurrentHeaderRaw(): Future[(String, HexHeight)] = {
    for {
      bestBlockHash <- rpcCli.getBestBlockHash
      hexHeight <- getBlockHeaderRaw(bestBlockHash)
    } yield {
      (bestBlockHash.hex, hexHeight)
    }
  }

  def checkForNewBlockchainTip(): Future[(Boolean, HexHeight)] = {
    for {
      (newBestBlockhash, hexHeight) <- getCurrentHeaderRaw()
    } yield {
      val isTipNew = !bestBlockHash.getAndUpdate(_ => Some(newBestBlockhash)).contains(newBestBlockhash)
      (isTipNew, hexHeight)
    }
  }

  def relayFee(): BigDecimal = {
    val networkInfo = wrap(rpcCli.getNetworkInfo)
    networkInfo.relayfee.toBigDecimal
  }

  def serverBanner(monitorState: TransactionMonitorState): String = {
    wrap(for {
      networkInfo <- rpcCli.getNetworkInfo
      blockchainInfo <- rpcCli.getBlockChainInfo
      uptime <- rpcCli.uptime
      //nettotals <- rpcCli.getNetTotals
    } yield {
      val uptimeDays = uptime.toInt / 86400
      val numWallets = monitorState.deterministicWallets.size
      val numAddresses = monitorState.addressHistory.m.size

      val banner = s"""Welcome to EPSMI $SERVER_VERSION (based on Electrum Personal Server).""" + "\n" +
        "\n" +
        s"""Monitoring $numWallets deterministic wallet(s), $numAddresses addresses.""" +
        "\n" +
        s"""Connected bitcoin node: ${networkInfo.subversion}""" + "\n" +
        s"""Uptime: ${uptimeDays} days""" + "\n" +
        s"""Peers: ${networkInfo.connections}""" + "\n" +
//        s"""Download: ${nettotals.totalbytesrecv} (${nettotals.totalbytesrecv/uptimeDays} per day)""" + "\n" +
//        s"""Upload: ${nettotals.totalbytessent} (${nettotals.totalbytessent/uptimeDays} per day)""" + "\n" +
        s"""Blocksonly: ${!networkInfo.localrelay}""" + "\n" +
        s"""Pruning: ${blockchainInfo.pruned}""" + "\n" +
        s"""Blockchain size: ${blockchainInfo.size_on_disk}""" + "\n" +
        """https://github.com/miloszm/epsmi""" + "\n" +
        "\n" +
        "\n" +
        """Donate to help maintain and improve EPSMI:""" + "\n" +
        s"""$DONATION_ADDRESS""" + "\n" +
        "\n" +
        "\n"
      banner
    })
  }

  //  #algorithm copied from the relevant place in ElectrumX
  //  #https://github.com/kyuupichan/electrumx/blob/e92c9bd4861c1e35989ad2773d33e01219d33280/server/mempool.py
  private def feeHistogram(mempool: Map[DoubleSha256DigestBE, GetMemPoolResult]): Array[Array[Int]] = {
    val feeHistogram = mempool.values.collect { case memPoolResult if memPoolResult.fee.isDefined =>
      memPoolResult.fee.get.toBigDecimal * 100000000 -> memPoolResult.size
    }.toList.sortWith((a,b) => a._1 < b._1)
    @tailrec
    def go(histo: List[(BigDecimal, Int)], binSize: Double, r: Double, size: Int, acc: List[(BigDecimal, Int)]): List[(BigDecimal, Int)] = histo match {
      case Nil => acc
      case (feeRate, s)::xs =>
        val sz = size + s
        if ((sz + r) > binSize)
          go(xs, binSize*1.1, r + sz - binSize, 0, acc :+ (feeRate, s))
        else
          go(xs, binSize, r, sz, acc)
    }
    val result = go(feeHistogram, 100000.0, 0, 0, Nil)
    result.map{case(feeRate, sz) => Array(feeRate, BigDecimal(sz)).map(_.toInt)}.toArray
  }

  def mempoolGetFeeHistogram(): Array[Array[Int]] = {
    if (config.enableMempoolFeeHistogram){
      val start = System.currentTimeMillis()
      val mempool: Map[DoubleSha256DigestBE, GetMemPoolResult] = wrap(rpcCli.getRawMemPoolWithTransactions)
      val stop = System.currentTimeMillis()
      if ((stop-start) > Constants.MEMPOOL_WARNING_DURATION*1000){
        printedSlowMempoolWarning.compareAndSet(false, {
          def f(u: Unit): Boolean = { true }
          f(logger.warn(s"Mempool very large resulting in slow response by server (${(stop-start)/1000} seconds)." +
            "Consider setting 'enable_mempool_fee_histogram = false'"))
        })
      }
      feeHistogram(mempool)
    } else {
      Array(Array(0,0))
    }
  }

  def blockchainTransactionBroadcast(txhex: String): String = {
    val transaction = Transaction.fromHex(txhex)
    val txReport = wrap(rpcCli.testMempoolAccept(transaction))
    if (txReport.allowed){
      logger.info(s"broadcasting tx ${txReport.txid}")
      if (config.broadcastMethod == OwnNode){
        val localRelay = wrap(rpcCli.getNetworkInfo).localrelay
        if (localRelay){
          wrap(rpcCli.sendRawTransaction(transaction))
          txReport.txid.hex
        } else {
          logger.warn("Transaction broadcasting disabled when blocksonly")
          throw new IllegalStateException("broadcast disabled when using blocksonly")
        }
      } else {
        throw new IllegalArgumentException(s"unsupported broadcast method: ${config.broadcastMethod}")
      }
    } else {
      throw new IllegalStateException(s"broadcast not possible for the following reject reason: ${txReport.rejectReason.getOrElse("unknown")}")
    }
  }

}
