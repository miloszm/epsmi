package com.mhm.api4electrum

import java.lang.reflect.Method
import java.util

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.googlecode.jsonrpc4j.JsonRpcInterceptor
import com.mhm.api4electrum.Api4ElectrumCore.{estimateSmartFee, getBlockHeader, getBlockHeaderHash, getBlockChunk, getBlockHeaders}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
import scala.math.BigDecimal.RoundingMode
import scala.util.Try

/**
 * This class conforms to json rpc requirements.
 * Uses Api4ElectrumCore for everything else.
 */

class Api4ElectrumImpl extends Api4Electrum {
  override def serverVersion(v1: String, v2: String): Array[String] = {
    Array("epsmi 0.0.2")
  }
  override def blockchainBlockHeader(height: Int): String = {
    Try(Await.result(getBlockHeaderHash(height), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalArgumentException(s"height $height out of range")
      },
      identity
    )
  }

  override def blockchainBlockGetHeader(height: Int): HeaderResult = {
    Try(Await.result(getBlockHeader(height), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalArgumentException(s"height $height out of range")
      },
      a =>
        a
    )
  }

  override def estimateFee(waitBlocks: Int): BigDecimal = {
    Try(Await.result(estimateSmartFee(waitBlocks), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"estimate fee for $waitBlocks block(s) wait failed")
      },
      a =>
        a.setScale(8, RoundingMode.FLOOR)
    )
  }

  override def blockchainBlockGetChunk(index: Int): String = {
    Try(Await.result(getBlockChunk(index), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get block chunk for index $index failed")
      },
      identity
    )
  }

  override def blockchainBlockHeaders(startHeight: Int, count: Int): BlockHeadersResult = {
    Try(Await.result(getBlockHeaders(startHeight, count), Duration(20, SECONDS))).fold(
      { t =>
        println(s"server caught: $t")
        throw new IllegalStateException(s"get block headers for start height $startHeight, count $count failed")
      },
      identity
    )
  }
}
