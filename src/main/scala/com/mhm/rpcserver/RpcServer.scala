package com.mhm.rpcserver

import java.io.File
import java.net.Socket

import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcBasicServer, JsonRpcClient, JsonRpcMethod, JsonRpcServer, ProxyUtil}
import com.mhm.securesocket.SecureSocketMetaFactory
import javax.net.ServerSocketFactory
import javax.net.ssl.SSLServerSocket

trait ElectrumService {
  @JsonRpcMethod("server.version")
  def serverVersion(v1: String, v2: String): Array[String]
  @JsonRpcMethod("blockchain.block.header")
  def blockchainBlockHeader(p1: Int): String
}

class ElectrumServiceImpl extends ElectrumService {
  override def serverVersion(v1: String, v2: String): Array[String] = {
    return Array("epsmi 0.0.2")
  }
  override def blockchainBlockHeader(p1: Int): String = {
    ???
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
  val serverSocketFactory = SecureSocketMetaFactory.createServerSocketFactory()
  val serverSocket = serverSocketFactory.createServerSocket(port).asInstanceOf[SSLServerSocket]
  serverSocket.setNeedClientAuth(true)
  val streamServer = new StreamServer(jsonRpcServer, maxThreads, serverSocket)

  streamServer.start()

  println(s"server started on port $port")

  Thread.sleep(36000000L)

  streamServer.stop()

}
