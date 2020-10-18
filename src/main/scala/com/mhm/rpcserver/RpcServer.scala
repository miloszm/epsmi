package com.mhm.rpcserver

import java.lang.reflect.Method
import java.util

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{InvocationListener, JsonRpcBasicServer, RequestInterceptor}
import com.mhm.api4electrum.{Api4Electrum, Api4ElectrumImpl}
import com.mhm.securesocket.SecureSocketMetaFactory
import javax.net.ssl.SSLServerSocket

object RpcServer extends App {

  val service = new Api4ElectrumImpl
  val jsonRpcServer = new JsonRpcBasicServer(service, classOf[Api4Electrum])

  val requestInterceptor = new RequestInterceptor {
    override def interceptRequest(request: JsonNode): Unit = {
      println(s"request intercepted:$request")
      // example: {"id":"1049080132","jsonrpc":"2.0","method":"blockchain.transaction.id_from_pos","params":[652742,5,false]}
      // when method is 'blockchain.transaction.id_from_pos' and third parameter is true
      //    change the method to 'blockchain.transaction.id_from_pos_merkle_true'
      val method = request.get("method")
      if (method.asText() == "blockchain.transaction.id_from_pos"){
        val params = request.get("params")
        val par3 = params.get(2)
        if (par3.asBoolean())
          request.asInstanceOf[ObjectNode].put("method", "blockchain.transaction.id_from_pos_merkle_true")
      }
    }
  }

//  val invocationListener = new InvocationListener {
//    override def willInvoke(method: Method, arguments: util.List[JsonNode]): Unit = {
//      println(s"willInvoke: method=$method args=$arguments")
//    }
//    override def didInvoke(method: Method, arguments: util.List[JsonNode], result: Any, t: Throwable, duration: Long): Unit = {
//      println(s"didInvoke: method=$method args=$arguments result=$result duration=$duration")
//    }
//  }

  jsonRpcServer.setRequestInterceptor(requestInterceptor)
//  jsonRpcServer.setInvocationListener(invocationListener)

  import java.net.InetAddress

  import com.googlecode.jsonrpc4j.StreamServer

  val maxThreads = 4
  val port = 1420
  val bindAddress = InetAddress.getByName("127.0.0.1")
  val backlog = 0
  val serverSocketFactory = SecureSocketMetaFactory.createServerSocketFactory()
  val serverSocket = serverSocketFactory.createServerSocket(port).asInstanceOf[SSLServerSocket]
  serverSocket.setNeedClientAuth(true)
  val streamServer = new StreamServer(jsonRpcServer, maxThreads, serverSocket)

  streamServer.start()

  println(s"server started on port $port")

  Thread.sleep(36000000L)

  streamServer.stop()

}
