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
//    val port = 50002
    val port = 1420
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

  def doServerVersion: Unit = {
    val epsmiClient = createClient()
    val client = epsmiClient.client

//    println
//    val result = client.serverVersion("1.9.5", "1.1")
//    println(s"result of server.version:")
//    println(s"size = ${result.length}")
//    result.zipWithIndex.foreach{ case (e, i) => println(s"$i = $e")}

//    println
//    val hexTry = Try(client.blockchainBlockHeader(652221))
//    hexTry.fold(
//      { e =>
//        println(s"json RPC exception caught: $e")
//        if (e.isInstanceOf[JsonRpcClientException]) println(s"code = ${e.asInstanceOf[JsonRpcClientException].getCode}")
//      },
//      { hex =>
//        println(s"result of blockchain.block.header:")
//        println(s"hex = $hex")
//      }
//    )

//    println
//    val header = client.blockchainBlockGetHeader(652221)
//    println(header)

//    println
//    val fee = client.estimateFee(1)
//    println(s"fee=$fee")

//    println
//    val chunk = client.blockchainBlockGetChunk(323)
//    println(s"chunk=$chunk")

//    println
//    val headersResult = client.blockchainBlockHeaders(652510, 512)
//    println(s"headers result = $headersResult")

    println
//    val transactionHex = client.blockchainTransactionGet("b850bd9f727888019ddd5481124b83c17b9dd263fe4c7c007a0a6c0f4c0f1573")
//    val transactionHex = client.blockchainTransactionGet("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
//    val transactionHex = client.blockchainTransactionGet("0e3e2357e806b6cdb1f70b54c3a3a17b6714ee1f0e68bebb44a74b1efd512098")
//    println(s"transaction result = $transactionHex")

//    println
//    val transactionId = client.blockchainTrIdFromPos(652742, 5, false)
//    println(s"trIdFromPos = $transactionId")
//    val trHex = client.blockchainTransactionGet(transactionId)
//    println(s"trHex for the above = $trHex")

//    println
//    val merkleResult: MerkleResult =
//      if (port == 50002) {
//        client.blockchainTrIdFromPosMerkleTrue(652742, 5, true)
//      } else {
//        val s = client.blockchainTrIdFromPos(652742, 5, true)
//        val objectMapper = new ObjectMapper()
//        objectMapper.readValue[MerkleResult](s, classOf[MerkleResult])
//      }
//    println(s"txHash= ${merkleResult.txHash}")
//    println("merkle=")
//    merkleResult.merkle.foreach{println(_)}

    println
    val txId4GetMerkle = client.blockchainTrIdFromPos(652742, 5, false) // otherwise it won't be found
    val merkle = client.blockchainTransactionGetMerkle(txId4GetMerkle)
    println(s"get merkle result = ")
    println(s"   blockHeight=${merkle.blockHeight}")
    println(s"   pos=${merkle.pos}")
    println(s"   merkle=")
    merkle.merkle.foreach{println(_)}

//    val socketInputStream = socket.getInputStream
//    while (true) {
//      println(socketInputStream.read().toChar)
//    }

    println
    // using the first address from the list of addresses shown by the original eps
    // you need to have actual wallet for that created, so this is really an
    // integration test
    val subscriptionResponse = client.blockchainScripthashSubcribe("12tohASdGUCDFvqaygaGbL7Jub7CiHdwa4")
    println(s"blockchainScripthashSubcribe result = ")
    println(s"   $subscriptionResponse")

//    Thread.sleep(60000L)
//
//    println("2nd time")
//    val txId4GetMerkle2 = client.blockchainTrIdFromPos(652742, 5, false) // otherwise it won't be found
//    val merkle2 = client.blockchainTransactionGetMerkle(txId4GetMerkle2)
//    println(s"get merkle result = ")
//    println(s"   blockHeight=${merkle2.blockHeight}")
//    println(s"   pos=${merkle2.pos}")
//    println(s"   merkle=")
//    merkle2.merkle.foreach{println(_)}
//
//    Thread.sleep(3000000L)
    epsmiClient.close()
  }

  private def createSocket(address: InetAddress, port: Int): Socket = {
    val socketFactory = SecureSocketMetaFactory.createSocketFactory()
    socketFactory.createSocket(address, port)
  }


  doServerVersion
}
