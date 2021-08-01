/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
