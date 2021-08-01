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
package com.mhm.rpcclient

import com.fasterxml.jackson.databind.ObjectMapper
import com.googlecode.jsonrpc4j.JsonRpcClientException
import com.mhm.api4electrum.{Api4Electrum, MerkleResult}
import com.mhm.rpcclient.RpcClient.createClient

import scala.util.Try

/**
  * Rudimentary test app for the client to call server APIs.
  * This code is for "manual" troubleshooting, it is not an automated test.
  * You need to run a server on the same port when running this test.
  * Make sure your local Electrum wallet is not running, as EPSMI
  * accepts only one connection at a time.
  */

object RpcClientTestApp extends App {
  val port = 50002

  private def callApiBlockchainScripthashGetBalance(client: Api4Electrum) = {
    println("=========== blockchain.scripthash.get_balance =============")
    val balanceResponse =
      client.blockchainScripthashGetBalance("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"balanceResponse = $balanceResponse")
  }

  private def callApiServerPing(client: Api4Electrum) = {
    println("=========== server.ping =============")
    val pingResponse = client.serverPing()
    println(s"ping response = $pingResponse")
  }

  private def callApiBlockchainScripthashGetHistory(client: Api4Electrum) = {
    println("=========== blockchain.scripthash.get_history =============")
    val history =
      client.blockchainScripthashGetHistory("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"blockchainScripthashGetHistory result = ${history.map(_.toString).mkString("|")}")
  }

  private def callApiBlockchainHeadersSubscribe(client: Api4Electrum) = {
    println("=========== blockchain.headers.subscribe =============")
    val headersSubscribeResult = client.blockchainHeadersSubcribe()
    println(s"blockchainHeadersSubcribe result = ")
    println(s"   hash=${headersSubscribeResult.hex}")
    println(s"   height=${headersSubscribeResult.height}")
  }

  private def callApiBlockchainScripthashSubscribe(client: Api4Electrum) = {
    println("=========== blockchain.scripthash.subscribe =============")
    // you need to have the actual wallet for that created
    val subscriptionResponse =
      client.blockchainScripthashSubcribe("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"blockchainScripthashSubcribe result = ")
    println(s"   $subscriptionResponse")
  }

  private def callApiBlockchainTransactionGetMerkle(client: Api4Electrum) = {
    println("=========== blockchain.transaction.get_merkle =============")
    val txId4GetMerkle = client.blockchainTrIdFromPos(652742, 5, false) // otherwise it won't be found
    val merkle         = client.blockchainTransactionGetMerkle(txId4GetMerkle, 0)
    println(s"get merkle result = ")
    println(s"   blockHeight=${merkle.block_height}")
    println(s"   pos=${merkle.pos}")
    println(s"   merkle=")
    merkle.merkle.foreach {
      println(_)
    }
  }

  private def callApiBlockchainTransactionIdFromPosMerkleTrue(client: Api4Electrum) = {
    println("=========== blockchain.transaction.id_from_pos_merkle_true =============")
    val merkleResult: MerkleResult = {
      val s = client.blockchainTrIdFromPos(652742, 5, true)
      new ObjectMapper().readValue[MerkleResult](s, classOf[MerkleResult])
    }
    println(s"txHash= ${merkleResult.tx_hash}")
    println("merkle=")
    merkleResult.merkle.foreach {
      println(_)
    }
  }

  private def callApiBlockchainTransactionIdFromPos(client: Api4Electrum) = {
    println("=========== blockchain.transaction.id_from_pos =============")
    val transactionId = client.blockchainTrIdFromPos(652742, 5, false)
    println(s"trIdFromPos = $transactionId")
    val trHex = client.blockchainTransactionGet(transactionId)
    println(s"trHex for the above = $trHex")
  }

  private def callApiBlockchainTransactionGet(client: Api4Electrum) = {
    println("=========== blockchain.transaction.get =============")
    val transactionHex =
      client.blockchainTransactionGet("0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098")
    println(s"transaction result = $transactionHex")
  }

  private def callApiBlockchainBlockHeaders(client: Api4Electrum) = {
    println("=========== blockchain.block.headers =============")
    val headersResult = client.blockchainBlockHeaders(652510, 512)
    println(s"headers result = $headersResult")
  }

  private def callApiBlockchainBlockGetChunk(client: Api4Electrum) = {
    println("=========== blockchain.block.get_chunk =============")
    val chunk = client.blockchainBlockGetChunk(323)
    println(s"chunk=$chunk")
  }

  private def callApiEstimateFee(client: Api4Electrum) = {
    println("=========== blockchain.estimatefee =============")
    val fee = client.estimateFee(1)
    println(s"fee=$fee")
  }

  private def callApiBlockchainBlockGetHeader(client: Api4Electrum) = {
    println("=========== blockchain.block.get_header =============")
    val header = client.blockchainBlockGetHeader(652221)
    println(header)
  }

  private def callApiBlockChainBlockHeader(client: Api4Electrum) = {
    println("=========== blockchain.block.header =============")
    val hexTry = Try(client.blockchainBlockHeader(652221))
    hexTry.fold({ e =>
      println(s"json RPC exception caught: $e")
      if (e.isInstanceOf[JsonRpcClientException]) println(s"code = ${e.asInstanceOf[JsonRpcClientException].getCode}")
    }, { hex =>
      println(s"result hex = $hex")
    })
  }

  private def callApiServerVersion(client: Api4Electrum) = {
    println("=========== server.version =============")
    val result = client.serverVersion("1.4", "1.4")
    println(s"result of server.version:")
    println(s"size = ${result.length}")
    result.zipWithIndex.foreach { case (e, i) => println(s"$i = $e") }
  }

  def performServerApiCalls: Unit = {
    val epsmiClient = createClient()
    val client      = epsmiClient.client

    callApiServerVersion(client)
    callApiBlockChainBlockHeader(client)
    callApiBlockchainBlockGetHeader(client)
    callApiEstimateFee(client)
    callApiBlockchainBlockGetChunk(client)
    callApiBlockchainBlockHeaders(client)
    callApiBlockchainTransactionGet(client)
    callApiBlockchainTransactionIdFromPos(client)
    callApiBlockchainTransactionIdFromPosMerkleTrue(client)
    callApiBlockchainTransactionGetMerkle(client)
    callApiBlockchainScripthashSubscribe(client)
    callApiBlockchainHeadersSubscribe(client)
    callApiBlockchainScripthashGetHistory(client)
    callApiServerPing(client)
    callApiBlockchainScripthashGetBalance(client)

    epsmiClient.close()
  }

  performServerApiCalls

}
