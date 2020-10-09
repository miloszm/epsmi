package com.mhm.securesocket

import java.io.File
import java.nio.file.Files
import java.security.SecureRandom
import java.security.cert.CertificateFactory

import javax.net.ssl.{KeyManagerFactory, SSLContext, SSLServerSocketFactory, SSLSocketFactory, TrustManagerFactory}

object SecureSocketMetaFactory {

  def createSocketFactory(certFile: File, keyFile: File): SSLSocketFactory = {
//    val sslContext: SSLContext = createSslServerContext(certFile, keyFile)
//    sslContext.getSocketFactory
    SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
  }

  def createServerSocketFactory(certFile: File, keyFile: File): SSLServerSocketFactory = {
//    val sslContext: SSLContext = createSslServerContext(certFile, keyFile)
//    sslContext.getServerSocketFactory
    SSLServerSocketFactory.getDefault.asInstanceOf[SSLServerSocketFactory]
  }

  private def createSslClientContext(certFile: File, keyFile: File) = {
    import java.io.FileInputStream
    import java.security.KeyStore
//    val is = new FileInputStream(certFile)
//
//    val cf = CertificateFactory.getInstance("X.509")
//    val caCert = cf.generateCertificate(is)
//    is.close()

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(new FileInputStream("/Users/miloszm/proj/epsmi/rpcserver2.jks"), "123456".toCharArray)

//    ks.setCertificateEntry("ca", caCert)

    tmf.init(ks)

    val sslContext = SSLContext.getInstance("TLS")
    sslContext.init(null, tmf.getTrustManagers, new SecureRandom())
    sslContext
  }

  private def createSslServerContext(certFile: File, keyFile: File) = {
    import java.io.FileInputStream
    import java.security.KeyStore
//    val is = new FileInputStream(certFile)
//
//    val cf = CertificateFactory.getInstance("X.509")
//    val caCert = cf.generateCertificate(is)
//    is.close()

    val tmf = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    val ks = KeyStore.getInstance(KeyStore.getDefaultType)
    ks.load(new FileInputStream("/Users/miloszm/proj/epsmi/rpcserver2.jks"), "123456".toCharArray)

    //ks.setCertificateEntry("ca", caCert)

    val kmf = KeyManagerFactory.getInstance("SunX509")
    kmf.init(ks, "123456".toCharArray)
    tmf.init(ks)

    val sslContext = SSLContext.getInstance("TLS")
    val keyManagers = kmf.getKeyManagers
//    sslContext.init(null, tmf.getTrustManagers, new SecureRandom())
    sslContext.init(keyManagers, null, null)
//    sslContext.init(keyManagers, tmf.getTrustManagers, new SecureRandom())
    sslContext
  }
}
