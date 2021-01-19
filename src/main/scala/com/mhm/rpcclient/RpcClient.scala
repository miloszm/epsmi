package com.mhm.rpcclient

import java.io.OutputStream
import java.net.{InetAddress, Socket}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, JsonRpcClientException, ProxyUtil}
import com.mhm.api4electrum.{Api4Electrum, HeaderResult, MerkleResult}
import com.mhm.securesocket.SecureSocketMetaFactory

import scala.util.Try


class LfObjectMapper extends ObjectMapper {
  override def writeValue(out: OutputStream, value: Any): Unit = {
    val str = value.toString + "\n"
    out.write(str.getBytes("UTF-8"))
  }
}


case class EpsmiClient(client: Api4Electrum, socket: Socket){
  def close(): Unit = socket.close()
}

object RpcClient extends App {
    val port = 50002
  var socket: Socket = null //TODO

  def createClient(port: Int = port): EpsmiClient = {
    socket = createSocket(InetAddress.getByName("127.0.0.1"), port)
    val rpcClient = new JsonRpcClient(new LfObjectMapper())
    val listener = new JsonRpcClient.RequestListener(){
      override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
        println(s"request=$request")
        val method = request.get("method")
        if (method.asText() == "blockchain.transaction.id_from_pos_merkle_true"){
          request.asInstanceOf[ObjectNode].put("method", "blockchain.transaction.id_from_pos")
        }
      }
      override def onBeforeResponseProcessed(client: JsonRpcClient, response: ObjectNode): Unit = {
        /**
         * for compatibility with EPS we want to remove 'data' in 'error'
         * so that JsonRpcClientException is returned rather than the original server exception
         */
        val e = response.get("error")
        if (e != null) {
          e.asInstanceOf[ObjectNode].remove("data")
        }
        println(s"response=$response")
      }
    }
    rpcClient.setRequestListener(listener)
    val client = ProxyUtil.createClientProxy(this.getClass.getClassLoader, classOf[Api4Electrum], rpcClient, socket)
    EpsmiClient(client, socket)
  }

  def performServerApiCalls: Unit = {
    val epsmiClient = createClient()
    val client = epsmiClient.client

    println("=========== server.version =============")
    val result = client.serverVersion("1.4", "1.4")
    println(s"result of server.version:")
    println(s"size = ${result.length}")
    result.zipWithIndex.foreach{ case (e, i) => println(s"$i = $e")}

    println("=========== blockchain.block.header =============")
    val hexTry = Try(client.blockchainBlockHeader(652221))
    hexTry.fold(
      { e =>
        println(s"json RPC exception caught: $e")
        if (e.isInstanceOf[JsonRpcClientException]) println(s"code = ${e.asInstanceOf[JsonRpcClientException].getCode}")
      },
      {
        hex => println(s"result hex = $hex")
      }
    )

    println("=========== blockchain.block.get_header =============")
    val header = client.blockchainBlockGetHeader(652221)
    println(header)

    println("=========== blockchain.estimatefee =============")
    val fee = client.estimateFee(1)
    println(s"fee=$fee")

    println("=========== blockchain.block.get_chunk =============")
    val chunk = client.blockchainBlockGetChunk(323)
    println(s"chunk=$chunk")

    println("=========== blockchain.block.headers =============")
    val headersResult = client.blockchainBlockHeaders(652510, 512)
    println(s"headers result = $headersResult")

    println("=========== blockchain.transaction.get =============")
    val transactionHex = client.blockchainTransactionGet("0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098")
    println(s"transaction result = $transactionHex")

    println("=========== blockchain.transaction.id_from_pos =============")
    val transactionId = client.blockchainTrIdFromPos(652742, 5, false)
    println(s"trIdFromPos = $transactionId")
    val trHex = client.blockchainTransactionGet(transactionId)
    println(s"trHex for the above = $trHex")

    println("=========== blockchain.transaction.id_from_pos_merkle_true =============")
    val merkleResult: MerkleResult = {
      val s = client.blockchainTrIdFromPos(652742, 5, true)
      new ObjectMapper().readValue[MerkleResult](s, classOf[MerkleResult])
    }
    println(s"txHash= ${merkleResult.tx_hash}")
    println("merkle=")
    merkleResult.merkle.foreach{println(_)}

    println("=========== blockchain.transaction.get_merkle =============")
    val txId4GetMerkle = client.blockchainTrIdFromPos(652742, 5, false) // otherwise it won't be found
    val merkle = client.blockchainTransactionGetMerkle(txId4GetMerkle, 0)
    println(s"get merkle result = ")
    println(s"   blockHeight=${merkle.block_height}")
    println(s"   pos=${merkle.pos}")
    println(s"   merkle=")
    merkle.merkle.foreach{println(_)}

    println("=========== blockchain.scripthash.subscribe =============")
    // you need to have actual wallet for that created
    val subscriptionResponse = client.blockchainScripthashSubcribe("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"blockchainScripthashSubcribe result = ")
    println(s"   $subscriptionResponse")

    println("=========== blockchain.headers.subscribe =============")
    val headersSubscribeResult = client.blockchainHeadersSubcribe()
    println(s"blockchainHeadersSubcribe result = ")
    println(s"   hash=${headersSubscribeResult.hex}")
    println(s"   height=${headersSubscribeResult.height}")

    println("=========== blockchain.scripthash.get_history =============")
    val history = client.blockchainScripthashGetHistory("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"blockchainScripthashGetHistory result = ${history.map(_.toString).mkString("|")}")

    println("=========== server.ping =============")
    val pingResponse = client.serverPing()
    println(s"ping response = $pingResponse")

    println("=========== blockchain.scripthash.get_balance =============")
    val balanceResponse = client.blockchainScripthashGetBalance("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    println(s"balanceResponse = $balanceResponse")

    epsmiClient.close()
  }

  private def createSocket(address: InetAddress, port: Int): Socket = {
    val socketFactory = SecureSocketMetaFactory.createSocketFactory()
    socketFactory.createSocket(address, port)
  }

  performServerApiCalls
}
