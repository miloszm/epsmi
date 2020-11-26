package com.mhm.main

import com.mhm.bitcoin.TransactionMonitor
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.mhm.rpcserver.RpcServer.port
import com.typesafe.config.ConfigFactory

object Main extends App {

  def doMain(): Unit = {
    val config = ConfigFactory.load()
    val scriptPubKeysToMonitorResult = new Setup(BitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

    val transactionMonitor = new TransactionMonitor(BitcoinSConnector.rpcCli, false)

    val monitorState = transactionMonitor.buildAddressHistory(
      scriptPubKeysToMonitorResult.spksToMonitor,
      scriptPubKeysToMonitorResult.wallets
    )

    val server = RpcServer.startServer(1420 /*, transactionMonitor, monitorState*/ )

    println(s"server started on port $port")

    Thread.sleep(36000000L)

    server.stop()
  }

}
