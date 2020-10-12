package com.mhm.rpcclient

import java.io.OutputStream
import java.net.{InetAddress, Socket}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, JsonRpcClientException, ProxyUtil}
import com.mhm.api4electrum.{Api4Electrum, HeaderResult}
import com.mhm.securesocket.SecureSocketMetaFactory

import scala.util.Try


class LfObjectMapper extends ObjectMapper {
  override def writeValue(out: OutputStream, value: Any): Unit = {
    val str = value.toString + "\n"
    out.write(str.getBytes("UTF-8"))
  }
}


object RpcClient extends App {

  def doServerVersion: Unit = {

//    val port = 50002
    val port = 1420

    val socket = createSocket(InetAddress.getByName("127.0.0.1"), port)
    val rpcClient = new JsonRpcClient(new LfObjectMapper())
    val listener = new JsonRpcClient.RequestListener(){
      override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
        println(s"request=$request")
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

    println
    val chunk = client.blockchainBlockGetChunk(323)
    println(s"chunk=$chunk")

    socket.close()
  }

  private def createSocket(address: InetAddress, port: Int): Socket = {
    val socketFactory = SecureSocketMetaFactory.createSocketFactory()
    socketFactory.createSocket(address, port)
  }


  doServerVersion
}
