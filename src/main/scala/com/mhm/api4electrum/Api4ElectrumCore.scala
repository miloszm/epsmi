package com.mhm.api4electrum

import java.nio.ByteBuffer

import com.mhm.connectors.BitcoinSConnector.{ec, rpcCli}
import com.mhm.util.EpsmiDataUtil.{byteVectorOrZeroToArray, byteVectorToArray, intToArray, uint32ToArray}
import javax.xml.bind.DatatypeConverter
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.FeeEstimationMode

import scala.concurrent.Future
import scala.math.BigDecimal.RoundingMode

/**
 * This class is futurized and does not necessarily conform
 * to json rpc requirements.
 */

object Api4ElectrumCore {
  def getBlockHeaderHash(blockHeight: Int): Future[String] = {
    for {
      blockHash <- rpcCli.getBlockHash(blockHeight)
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
      headHex.toLowerCase
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
}
