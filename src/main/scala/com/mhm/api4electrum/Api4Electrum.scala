/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.api4electrum

import com.fasterxml.jackson.annotation.{JsonCreator, JsonProperty}
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.googlecode.jsonrpc4j.{JsonRpcError, JsonRpcErrors, JsonRpcMethod}
import com.mhm.common.model.HistoryElement

import scala.beans.BeanProperty

@JsonSerialize
case class HeaderResult(@BeanProperty @JsonProperty("block_height") block_height: Int,
                        @BeanProperty @JsonProperty("prev_block_hash") prev_block_hash: String,
                        @BeanProperty @JsonProperty("timestamp") timestamp: Long,
                        @BeanProperty @JsonProperty("merkle_root") merkle_root: String,
                        @BeanProperty @JsonProperty("version") version: Long,
                        @BeanProperty @JsonProperty("nonce") nonce: Long,
                        @BeanProperty @JsonProperty("bits") bits: Long)

@JsonSerialize
case class BlockHeadersResult(@BeanProperty @JsonProperty("hex") hex: String,
                              @BeanProperty @JsonProperty("count") count: Int,
                              @BeanProperty @JsonProperty("max") max: Int,
)

@JsonSerialize
case class MerkleResult(@BeanProperty @JsonProperty("tx_hash") tx_hash: String,
                        @BeanProperty @JsonProperty("merkle") merkle: Array[String])

@JsonSerialize
case class GetMerkleResult(@BeanProperty @JsonProperty("block_height") block_height: Int,
                           @BeanProperty @JsonProperty("pos") pos: Int,
                           @BeanProperty @JsonProperty("merkle") merkle: Array[String])

@JsonSerialize
case class HeadersSubscribeResult(@BeanProperty @JsonProperty("height") height: Int,
                                  @BeanProperty @JsonProperty("hex") hex: String)

@JsonSerialize
case class HistoryItem(@BeanProperty @JsonProperty("height") height: Int,
                       @BeanProperty @JsonProperty("tx_hash") tx_hash: String,
                       @BeanProperty @JsonProperty("fee") fee: Long)

@JsonSerialize
case class GetBalanceResult(@BeanProperty @JsonProperty("confirmed") confirmed: BigDecimal,
                            @BeanProperty @JsonProperty("unconfirmed") unconfirmed: BigDecimal)

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

  @JsonRpcMethod("blockchain.transaction.get")
  def blockchainTransactionGet(txid: String): String

  @JsonRpcMethod("blockchain.transaction.id_from_pos")
  def blockchainTrIdFromPos(height: Int, txPos: Int, merkle: Boolean): String

  // for client only - will be intercepted by client and changed to blockchain.transaction.id_from_pos
  // (for the case of merkle = true)
  @JsonRpcMethod("blockchain.transaction.id_from_pos_merkle_true")
  def blockchainTrIdFromPosMerkleTrue(height: Int, txPos: Int, merkle: Boolean): MerkleResult

  @JsonRpcMethod("blockchain.transaction.get_merkle")
  def blockchainTransactionGetMerkle(txid: String, height: Int): GetMerkleResult

  @JsonRpcMethod("blockchain.scripthash.subscribe")
  def blockchainScripthashSubcribe(sh: String): String

  @JsonRpcMethod("blockchain.headers.subscribe")
  def blockchainHeadersSubcribe(): HeadersSubscribeResult

  @JsonRpcMethod("blockchain.scripthash.get_history")
  def blockchainScripthashGetHistory(sh: String): Array[HistoryItem]

  @JsonRpcMethod("server.ping")
  def serverPing(): Unit

  @JsonRpcMethod("blockchain.scripthash.get_balance")
  def blockchainScripthashGetBalance(sh: String): GetBalanceResult

  @JsonRpcMethod("server.peers.subscribe")
  def serverPeersSubscribe(): Array[String]

  @JsonRpcMethod("server.donation_address")
  def serverDonationAddress(): String

  @JsonRpcMethod("mempool.get_fee_histogram")
  def mempoolGetFeeHistogram(): Array[Array[Int]]

  @JsonRpcMethod("blockchain.relayfee")
  def blockchainRelayFee(): BigDecimal

  @JsonRpcMethod("server.banner")
  def serverBanner(): String

  @JsonRpcMethod("blockchain.transaction.broadcast")
  def blockchainTransactionBroadcast(txhex: String): String

}
