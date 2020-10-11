package com.mhm.api4electrum

import com.googlecode.jsonrpc4j.{JsonRpcError, JsonRpcErrors, JsonRpcMethod}

trait Api4Electrum {
  @JsonRpcMethod("server.version")
  def serverVersion(v1: String, v2: String): Array[String]

  /**
   * this annotation overrides error code but not the exception
   */
  @JsonRpcErrors(Array(new JsonRpcError(exception = classOf[IllegalArgumentException], code = -1)))
  @JsonRpcMethod("blockchain.block.header")
  def blockchainBlockHeader(p1: Int): String
}
