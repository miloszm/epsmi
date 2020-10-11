package com.mhm.api4electrum

import com.googlecode.jsonrpc4j.JsonRpcMethod

trait Api4Electrum {
  @JsonRpcMethod("server.version")
  def serverVersion(v1: String, v2: String): Array[String]
  @JsonRpcMethod("blockchain.block.header")
  def blockchainBlockHeader(p1: Int): String
}
