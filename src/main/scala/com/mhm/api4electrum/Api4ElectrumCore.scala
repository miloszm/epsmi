package com.mhm.api4electrum

import java.nio.ByteBuffer

import com.mhm.connectors.BitcoinSConnector.{ec, rpcCli}
import com.mhm.util.EpsmiDataUtil.{byteVectorOrZeroToArray, byteVectorToArray, intToArray, uint32ToArray}
import javax.xml.bind.DatatypeConverter
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.FeeEstimationMode
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.annotation.tailrec
import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, Future}
import scala.math.BigDecimal.RoundingMode

/**
 * This class is futurized and does not necessarily conform
 * to json rpc requirements.
 */

object Api4ElectrumCore {
  def getBlockHeaderHash(blockHeight: Int): Future[String] = {
    for {
      blockHash <- rpcCli.getBlockHash(blockHeight)
      (blockHeaderHash, nextBlockHashOpt) <- getBlockHeaderHashFromBlockHash(blockHash)
    } yield {
      blockHeaderHash
    }
  }

  /**
   * returns block header hash and next block hash
   */
  private def getBlockHeaderHashFromBlockHash(blockHash: DoubleSha256DigestBE): Future[(String,Option[DoubleSha256DigestBE])] = {
    for {
      blockHeader <- rpcCli.getBlockHeader(blockHash)
    } yield {
      val prevBlockHashArray = Array.fill[Byte](32)(0)

      blockHeader.previousblockhash match {
        case Some(b) => b.bytes.copyToArray(prevBlockHashArray, 0)
        case _ => ()
      }

      val merkleRootArray = Array.fill[Byte](32)(0)
      blockHeader.merkleroot.bytes.copyToArray(merkleRootArray, 0)

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
      (headHex.toLowerCase, blockHeader.nextblockhash)
    }
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


  private def getBlockHeaders(startHeight: Int, count: Int): Future[String] = {
    @tailrec
    def go(blockHashOpt: Option[DoubleSha256DigestBE], headerHashes: List[String], count: Int): List[String] = {
      if (count > 0 && blockHashOpt.isDefined) {
        val (headerHash, nextBlockHashOpt) = Await.result(getBlockHeaderHashFromBlockHash(blockHashOpt.get), Duration(20, SECONDS))
        go(nextBlockHashOpt, headerHash +: headerHashes, count - 1)
      } else {
        headerHashes
      }
    }
    if (count <= 0){
      Future.successful("")
    } else {
      val firstBlockHash = Await.result(rpcCli.getBlockHash(startHeight), Duration(20, SECONDS))
      val headerHashes = go(Some(firstBlockHash), Nil, count)
      Future.successful(headerHashes.reverse.mkString)
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
      headersHex <- getBlockHeaders(startHeight, count)
    } yield {
      headersHex
    }
  }
}
