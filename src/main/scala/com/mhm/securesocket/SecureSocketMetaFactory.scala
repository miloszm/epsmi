package com.mhm.securesocket

import javax.net.ssl.{SSLServerSocketFactory, SSLSocketFactory}

/**
 * note that this approach requires the following VM options to be present:
 * -Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks
 * -Djavax.net.ssl.keyStorePassword=123456
 * -Djavax.net.ssl.trustStore=/Users/miloszm/proj/epsmi/rpcserver2.jks
 * rpcserver2.jks was prepared using openssl and keytool,
 * as described in keystore_creation_readme.txt
 * trustStore is set to the same as keyStore, so that our certificate
 * is self-certifying, it is its own certification authority
 */
object SecureSocketMetaFactory {

  def createSocketFactory(): SSLSocketFactory = {
    SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
  }

  def createServerSocketFactory(): SSLServerSocketFactory = {
    SSLServerSocketFactory.getDefault.asInstanceOf[SSLServerSocketFactory]
  }

}
