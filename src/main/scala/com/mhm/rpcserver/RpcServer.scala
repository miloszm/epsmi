package com.mhm.rpcserver

import java.net.Socket

import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcBasicServer, JsonRpcClient, JsonRpcServer, ProxyUtil}
import javax.net.ServerSocketFactory

trait ElectrumService {
  def testMe(x: Int): Int = x
}

class ElectrumServiceImpl extends ElectrumService {
  override def testMe(x: Int): Int = {
    println(s"testme called with $x")
    x
  }
}

object RpcServer extends App {

  val service = new ElectrumServiceImpl
  val jsonRpcServer = new JsonRpcBasicServer(service, classOf[ElectrumService])

  import com.googlecode.jsonrpc4j.StreamServer
  import java.net.InetAddress

  val maxThreads = 4
  val port = 1420
  val bindAddress = InetAddress.getByName("127.0.0.1")
  val backlog = 0
  val serverSocket = ServerSocketFactory.getDefault.createServerSocket(port, backlog, bindAddress)
  val streamServer = new StreamServer(jsonRpcServer, maxThreads, serverSocket)

  streamServer.start()

  val socket = new Socket(serverSocket.getInetAddress, serverSocket.getLocalPort)
  val rpcClient = new JsonRpcClient()
  val listener = new JsonRpcClient.RequestListener(){
    override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
      println(request)
    }
    override def onBeforeResponseProcessed(client: JsonRpcClient, response: ObjectNode): Unit = {
      println(response)
    }
  }
  rpcClient.setRequestListener(listener)
  val client = ProxyUtil.createClientProxy(this.getClass.getClassLoader, classOf[ElectrumService], rpcClient, socket)
  for (i <- 0 until 5) {
    client.testMe(i)
  }
  socket.close()
  streamServer.stop()

}
