package com.mhm.main

import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory

object Main extends App {
  val port = 50002
//  val port = 1420

  def doMain(): Unit = {
    val config = ConfigFactory.load()
    val scriptPubKeysToMonitorResult = new Setup(BitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

    val transactionMonitor = TransactionMonitorFactory.create(BitcoinSConnector.rpcCli)

    val monitorState = transactionMonitor.buildAddressHistory(
      scriptPubKeysToMonitorResult.spksToMonitor,
      scriptPubKeysToMonitorResult.wallets
    )

    val server = RpcServer.startServer(port, transactionMonitor, monitorState)

    println(s"server started on port $port")

    Thread.sleep(36000000L)

    server.stop()
  }

  doMain

}
