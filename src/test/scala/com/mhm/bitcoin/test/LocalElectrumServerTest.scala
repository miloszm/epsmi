package com.mhm.bitcoin.test

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, File}
import java.net.{InetAddress, Socket, URL}
import java.security.SecureRandom
import java.util
import java.util.Dictionary

import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcClient, JsonRpcHttpClient}
import com.mhm.rpcserver.RpcServer.serverSocket
import javax.net.ssl.{HostnameVerifier, SSLContext, SSLSession, TrustManagerFactory}

import scala.jdk.CollectionConverters._

object LocalElectrumServerTest extends App {
  val rpcClient = new JsonRpcHttpClient(new URL("https://localhost:50002"))

  import java.io.FileInputStream
  import java.io.InputStream
  import java.security.KeyStore
  import java.security.cert.CertificateFactory

  val is = new FileInputStream(new File("/Users/miloszm/proj/epsmi/cert.crt"))
  // You could get a resource as a stream instead.

  val cf = CertificateFactory.getInstance("X.509")
  val caCert = cf.generateCertificate(is)

  val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
  val ks = KeyStore.getInstance(KeyStore.getDefaultType)
  ks.load(null) // You don't need the KeyStore instance to come from a file.

  ks.setCertificateEntry("ca", caCert)

  tmf.init(ks)

  val sslContext = SSLContext.getInstance("TLS")
  sslContext.init(null, tmf.getTrustManagers, new SecureRandom())
  rpcClient.setSslContext(sslContext)


  javax.net.ssl.HttpsURLConnection.setDefaultHostnameVerifier(new HostnameVerifier() {
    override def verify(hostname: String, sslSession: SSLSession): Boolean = hostname == "localhost"
  })

  val listener = new JsonRpcClient.RequestListener(){
    override def onBeforeRequestSent(client: JsonRpcClient, request: ObjectNode): Unit = {
      println(request)
      println(request.toString.map(a => "" + a.toInt).mkString("|"))
    }
    override def onBeforeResponseProcessed(client: JsonRpcClient, response: ObjectNode): Unit = {
      println(response)
    }
  }

  rpcClient.setRequestListener(listener)


  val params = Array("1.9.5", "0.6")
  val props = new util.HashMap[String, String]()
  props.put("a", "b")
  val response = rpcClient.invoke("server.version", null, classOf[String])
  println(s"response = $response")
}
