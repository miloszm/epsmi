package com.mhm.rpcclient

import java.io.{File, OutputStream}
import java.net.{InetAddress, Socket}
import java.security.SecureRandom
import java.security.cert.CertificateFactory

import com.fasterxml.jackson.databind.ObjectMapper
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, JsonRpcMethod, ProxyUtil}
import javax.net.ssl.{SSLContext, TrustManagerFactory}


trait ElectrumService2 {
  @JsonRpcMethod("server.version")
  def serverVersion(v1: String, v2: String): Array[String]
}


class LfObjectMapper extends ObjectMapper {
  override def writeValue(out: OutputStream, value: Any): Unit = {
    val str = value.toString + "\n"
    out.write(str.getBytes("UTF-8"))
  }
}


object RpcClient extends App {

  def doServerVersion: Unit = {

    val port = 50002

    val socket = createSocket(InetAddress.getByName("127.0.0.1"), port)
    val rpcClient = new JsonRpcClient(new LfObjectMapper())
    val listener = new JsonRpcClient.RequestListener(){
      override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
        println(request)
      }
      override def onBeforeResponseProcessed(client: JsonRpcClient, response: ObjectNode): Unit = {
        println(response)
      }
    }
    rpcClient.setRequestListener(listener)
    val client = ProxyUtil.createClientProxy(this.getClass.getClassLoader, classOf[ElectrumService2], rpcClient, socket)
    val result = client.serverVersion("1.9.5", "1.1")
    println(s"result size = ${result.length}")
    result.zipWithIndex.foreach{ case (e, i) => println(s"$i = $e")}
    socket.close()
  }

  private def createSocket(address: InetAddress, port: Int): Socket = {
    import java.io.FileInputStream
    import java.security.KeyStore
    val is = new FileInputStream(new File("/Users/miloszm/proj/epsmi/cert.crt"))

    val cf = CertificateFactory.getInstance("X.509")
    val caCert = cf.generateCertificate(is)

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(null) // You don't need the KeyStore instance to come from a file.

    ks.setCertificateEntry("ca", caCert)

    tmf.init(ks)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, tmf.getTrustManagers, new SecureRandom())

    val socketFactory = sslContext.getSocketFactory

    val socket = socketFactory.createSocket(address, port)
    socket
  }

  doServerVersion
}
