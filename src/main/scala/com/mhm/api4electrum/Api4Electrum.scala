package com.mhm.api4electrum

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.googlecode.jsonrpc4j.{JsonRpcError, JsonRpcErrors, JsonRpcMethod}

import scala.beans.BeanProperty

@JsonSerialize
case class HeaderResult (
  @BeanProperty @JsonProperty("block_height") blockHeight: Int,
  @BeanProperty @JsonProperty("prev_block_hash") prevBlockHash: String,
  @BeanProperty @JsonProperty("timestamp") timestamp: Long,
  @BeanProperty @JsonProperty("merkle_root") merkleRoot: String,
  @BeanProperty @JsonProperty("version") version: Long,
  @BeanProperty @JsonProperty("nonce") nonce: Long,
  @BeanProperty @JsonProperty("bits") bits: Long
)

@JsonSerialize
case class BlockHeadersResult (
  @BeanProperty @JsonProperty("hex") hex: String,
  @BeanProperty @JsonProperty("count") count: Int,
  @BeanProperty @JsonProperty("max") max: Int,
)

trait Api4Electrum {
  @JsonRpcMethod("server.version")
  def serverVersion(v1: String, v2: String): Array[String]

  /**
   * this annotation overrides error code but not the exception
   */
  @JsonRpcErrors(Array(new JsonRpcError(exception = classOf[IllegalArgumentException], code = -1)))
  @JsonRpcMethod("blockchain.block.header")
  def blockchainBlockHeader(height: Int): String

  @JsonRpcMethod("blockchain.block.get_header")
  def blockchainBlockGetHeader(height: Int): HeaderResult

  @JsonRpcMethod("blockchain.estimatefee")
  def estimateFee(waitBlocks: Int): BigDecimal

  @JsonRpcMethod("blockchain.block.get_chunk")
  def blockchainBlockGetChunk(index: Int): String

  @JsonRpcMethod("blockchain.block.headers")
  def blockchainBlockHeaders(startHeight: Int, count: Int): BlockHeadersResult
}
