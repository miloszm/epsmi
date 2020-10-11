package com.mhm.rpcserver

import com.googlecode.jsonrpc4j.JsonRpcBasicServer
import com.mhm.api4electrum.{Api4Electrum, Api4ElectrumImpl}
import com.mhm.securesocket.SecureSocketMetaFactory
import javax.net.ssl.SSLServerSocket

object RpcServer extends App {

  val service = new Api4ElectrumImpl
  val jsonRpcServer = new JsonRpcBasicServer(service, classOf[Api4Electrum])

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
