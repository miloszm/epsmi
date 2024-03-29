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
package com.mhm.rpcserver

import java.io.OutputStream
import java.lang.reflect.Method
import java.util

import com.fasterxml.jackson.databind.JsonNode
import com.fasterxml.jackson.databind.node.ObjectNode
import com.googlecode.jsonrpc4j.{JsonRpcBasicServer, JsonRpcInterceptor, RequestInterceptor, StreamServerWithHeartbeats}
import com.mhm.api4electrum.{Api4Electrum, Api4ElectrumCore, Api4ElectrumCoreConfig, Api4ElectrumImpl}
import com.mhm.bitcoin.{
  NoopTxsMonitorStateListener,
  TransactionMonitor,
  TransactionMonitorState,
  TxsMonitorStateListener
}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.securesocket.SecureSocketMetaFactory
import grizzled.slf4j.Logging
import javax.net.ssl.SSLServerSocket

import scala.concurrent.ExecutionContext.Implicits.global
import scala.jdk.CollectionConverters.SeqHasAsJava

object RpcServer extends Logging {
  val maxThreads = 1

  def startServer(
    transactionMonitor: TransactionMonitor,
    monitorState: TransactionMonitorState,
    coreConfig: Api4ElectrumCoreConfig,
    monitorStateListener: TxsMonitorStateListener = NoopTxsMonitorStateListener
  ): StreamServerWithHeartbeats = {
    val bitcoinSConnector =
      BitcoinSConnector(coreConfig.isTestnet, coreConfig.btcRpcUsername, coreConfig.btcRpcPassword)
    val service = new Api4ElectrumImpl(
      Api4ElectrumCore(bitcoinSConnector.rpcCli, coreConfig),
      transactionMonitor,
      monitorState,
      monitorStateListener
    )
    val jsonRpcServer = new JsonRpcBasicServer(service, classOf[Api4Electrum])

    val requestInterceptor = new RequestInterceptor {
      override def interceptRequest(request: JsonNode): Unit = {
        logger.info(s"=> $request")
        // example: {"id":"1049080132","jsonrpc":"2.0","method":"blockchain.transaction.id_from_pos","params":[652742,5,false]}
        // when method is 'blockchain.transaction.id_from_pos' and third parameter is true
        //    change the method to 'blockchain.transaction.id_from_pos_merkle_true'
        val method = request.get("method")
        if (method.asText() == "blockchain.transaction.id_from_pos") {
          val params = request.get("params")
          val par3   = params.get(2)
          if (par3.asBoolean())
            request.asInstanceOf[ObjectNode].put("method", "blockchain.transaction.id_from_pos_merkle_true")
        }
      }
    }

    val jsonRpcInterceptor = new JsonRpcInterceptor {
      override def preHandleJson(json: JsonNode): Unit = {
        //logger.info(s"===> $json")
      }

      override def preHandle(target: Any, method: Method, params: util.List[JsonNode]): Unit = {}

      override def postHandle(target: Any, method: Method, params: util.List[JsonNode], result: JsonNode): Unit = {}

      override def postHandleJson(json: JsonNode): Unit = {
        //logger.info(s"<= $json")
      }

      override def onHeartbeatConnected(outputStream: OutputStream): Unit = {
        service.triggerHeartbeatConnected(outputStream)
      }

      override def onHeartbeatListening(): Unit = {
        println("onHeartbeatListening!!")
      }
    }

    //  val invocationListener = new InvocationListener {
    //    override def willInvoke(method: Method, arguments: util.List[JsonNode]): Unit = {
    //      println(s"willInvoke: method=$method args=$arguments")
    //    }
    //    override def didInvoke(method: Method, arguments: util.List[JsonNode], result: Any, t: Throwable, duration: Long): Unit = {
    //      println(s"didInvoke: method=$method args=$arguments result=$result duration=$duration")
    //    }
    //  }

    jsonRpcServer.setRequestInterceptor(requestInterceptor)
    //  jsonRpcServer.setInvocationListener(invocationListener)

    jsonRpcServer.setInterceptorList(List(jsonRpcInterceptor).asJava)

    val serverSocketFactory = SecureSocketMetaFactory.createServerSocketFactory()
    val serverSocket        = serverSocketFactory.createServerSocket(coreConfig.port).asInstanceOf[SSLServerSocket]
    //serverSocket.setNeedClientAuth(true)
    val streamServer = new StreamServerWithHeartbeats(jsonRpcServer, maxThreads, serverSocket)

    try {
      streamServer.start()
    } catch {
      case e: Throwable =>
        logger.error(s"rcp server caught ${e.getClass.getCanonicalName} exception: ${e.getMessage}")
        throw e
    }

    streamServer
  }

}
