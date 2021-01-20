package com.mhm.rpcclient

import java.io.OutputStream
import java.net.{InetAddress, Socket}

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, ProxyUtil}
import com.mhm.api4electrum.Api4Electrum
import com.mhm.securesocket.SecureSocketMetaFactory

class LfObjectMapper extends ObjectMapper {
  override def writeValue(out: OutputStream, value: Any): Unit = {
    val str = value.toString + "\n"
    out.write(str.getBytes("UTF-8"))
  }
}

case class EpsmiClient(client: Api4Electrum, socket: Socket) {
  def close(): Unit = socket.close()
}

object RpcClient {
  val defaultPort = 50002

  def createClient(port: Int = defaultPort): EpsmiClient = {
    val socket    = createSocket(InetAddress.getByName("127.0.0.1"), port)
    val rpcClient = new JsonRpcClient(new LfObjectMapper())
    val listener = new JsonRpcClient.RequestListener() {
      override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
        println(s"request=$request")
        val method = request.get("method")
        if (method.asText() == "blockchain.transaction.id_from_pos_merkle_true") {
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

  private def createSocket(address: InetAddress, port: Int): Socket = {
    val socketFactory = SecureSocketMetaFactory.createSocketFactory()
    socketFactory.createSocket(address, port)
  }

}
