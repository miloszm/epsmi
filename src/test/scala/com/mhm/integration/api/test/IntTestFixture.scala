package com.mhm.integration.api.test

import com.googlecode.jsonrpc4j.StreamServerWithHeartbeats
import com.mhm.api4electrum.{Api4Electrum, Api4ElectrumCoreConfig}
import com.mhm.bitcoin.{OwnNode, TransactionMonitorFactory}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.main.Setup
import com.mhm.rpcclient.{EpsmiClient, RpcClient}
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory
import org.scalatest.{BeforeAndAfterAll, FlatSpecLike}

trait IntTestFixture extends FlatSpecLike with BeforeAndAfterAll {

  val EXTERNAL_EPS_PORT = 50002
  val EPSMI_PORT = 1420
  val port = EPSMI_PORT

  // -Djavax.net.ssl.keyStore=/Users/miloszm/proj/epsmi/rpcserver2.jks -Djavax.net.ssl.keyStorePassword=123456

  case class Fixture(server: StreamServerWithHeartbeats, epsmiClient: EpsmiClient) {
    def client(): Api4Electrum = epsmiClient.client
  }

  val config = ConfigFactory.load()
  val coreConfig = Api4ElectrumCoreConfig(enableMempoolFeeHistogram = false, broadcastMethod = OwnNode, port = port, isTestnet = false, btcRpcUsername = "foo", btcRpcPassword = "bar", initialImportCount = 1000)
  val scriptPubKeysToMonitorResult = new Setup(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

  val transactionMonitor = TransactionMonitorFactory.create(TestBitcoinSConnector.rpcCli)

  val monitorState = transactionMonitor.buildAddressHistory(
    scriptPubKeysToMonitorResult.spksToMonitor,
    scriptPubKeysToMonitorResult.wallets
  )

  lazy val fixture = Fixture(RpcServer.startServer(port, transactionMonitor, monitorState, coreConfig), RpcClient.createClient(port))

  override protected def afterAll(): Unit = {
    fixture.epsmiClient.close()
    fixture.server.stop()
    super.afterAll()
  }

}
