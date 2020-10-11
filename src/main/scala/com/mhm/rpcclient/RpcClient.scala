package com.mhm.rpcclient

import java.io.{File, OutputStream}
import java.net.{InetAddress, Socket}
import java.security.SecureRandom
import java.security.cert.CertificateFactory
import java.util

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, JsonRpcMethod, ProxyUtil}
import com.mhm.api4electrum.Api4Electrum
import com.mhm.securesocket.SecureSocketMetaFactory
import javax.net.ssl.{SSLContext, SSLSocketFactory, TrustManagerFactory}

import scala.jdk.CollectionConverters.CollectionHasAsScala


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
        //println(request)
      }
      override def onBeforeResponseProcessed(client: JsonRpcClient, response: ObjectNode): Unit = {
        //println(response)
      }
    }
    rpcClient.setRequestListener(listener)
    val client = ProxyUtil.createClientProxy(this.getClass.getClassLoader, classOf[Api4Electrum], rpcClient, socket)
    val result = client.serverVersion("1.9.5", "1.1")
    println(s"result of server.version:")
    println(s"size = ${result.length}")
    result.zipWithIndex.foreach{ case (e, i) => println(s"$i = $e")}
    println
    val hex = client.blockchainBlockHeader(652221)
    println(s"result of blockchain.block.header:")
    println(s"hex = $hex")
    socket.close()
  }

  private def createSocket(address: InetAddress, port: Int): Socket = {
    val socketFactory = SecureSocketMetaFactory.createSocketFactory()
    socketFactory.createSocket(address, port)
  }


  doServerVersion
}
