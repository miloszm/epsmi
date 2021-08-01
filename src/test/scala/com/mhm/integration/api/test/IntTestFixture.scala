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
package com.mhm.integration.api.test

import com.googlecode.jsonrpc4j.StreamServerWithHeartbeats
import com.mhm.api4electrum.{Api4Electrum, Api4ElectrumCoreConfig}
import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.common.model.OwnNode
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.main.SpksToMonitorFinder
import com.mhm.rpcclient.{EpsmiClient, RpcClient}
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}

trait IntTestFixture extends FlatSpecLike with BeforeAndAfterAll {

  val EXTERNAL_EPS_PORT = 50002
  val EPSMI_PORT        = 1420
  val port              = EPSMI_PORT

  // -Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks -Djavax.net.ssl.keyStorePassword=123456

  case class Fixture(server: StreamServerWithHeartbeats, epsmiClient: EpsmiClient) {
    def client(): Api4Electrum = epsmiClient.client
  }

  val config = ConfigFactory.load()

  val coreConfig = Api4ElectrumCoreConfig(
    enableMempoolFeeHistogram = false,
    broadcastMethod           = OwnNode,
    port                      = port,
    isTestnet                 = false,
    btcRpcUsername            = "foo",
    btcRpcPassword            = "bar",
    initialImportCount        = 1000
  )

  val scriptPubKeysToMonitorResult =
    new SpksToMonitorFinder(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

  val transactionMonitor = TransactionMonitorFactory.create(TestBitcoinSConnector.rpcCli)

  val monitorState = transactionMonitor
    .buildAddressHistory(scriptPubKeysToMonitorResult.spksToMonitor, scriptPubKeysToMonitorResult.wallets)

  lazy val fixture =
    Fixture(RpcServer.startServer(transactionMonitor, monitorState, coreConfig), RpcClient.createClient(port))

  override protected def afterAll(): Unit = {
    fixture.epsmiClient.close()
    fixture.server.stop()
    super.afterAll()
  }

}
