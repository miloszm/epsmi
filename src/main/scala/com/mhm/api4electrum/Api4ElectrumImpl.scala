package com.mhm.api4electrum

import java.lang.reflect.Method
import java.util

import com.fasterxml.jackson.databind.{JsonNode, ObjectMapper}
import com.googlecode.jsonrpc4j.JsonRpcInterceptor
import com.mhm.api4electrum.Api4ElectrumCore.{getBlockHeader, getBlockHeaderHash}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}
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

}
