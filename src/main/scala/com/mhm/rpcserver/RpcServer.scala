package com.mhm.rpcserver

import java.net.Socket

import com.googlecode.jsonrpc4j.{JsonRpcBasicServer, JsonRpcClient, JsonRpcServer, ProxyUtil}
import javax.net.ServerSocketFactory

trait MyService {
  def testme(x: Int): Int = x
}

class MyServiceImpl extends MyService {
  override def testme(x: Int): Int = {
    println(s"testme called with $x")
    x
  }
}

object RpcServer extends App {

  val service = new MyServiceImpl
  val jsonRpcServer = new JsonRpcBasicServer(service, classOf[MyService])

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
  val client = ProxyUtil.createClientProxy(this.getClass.getClassLoader, classOf[MyService], new JsonRpcClient(), socket)
  for (i <- 0 until 5) {
    client.testme(i)
  }
  socket.close()

  streamServer.stop()
}
