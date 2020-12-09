package com.mhm.main

import com.mhm.api4electrum.Api4ElectrumCoreConfig
import com.mhm.bitcoin.{OwnNode, TransactionMonitorFactory, UnsupportedBroadcastMethod}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory

object Main extends App {
  def doMain(): Unit = {
    val config = ConfigFactory.load()
    val isTestnet = config.getBoolean("epsmi.testnet")
    val (btcRpcUsername, btcRpcPassword) = (config.getString("epsmi.btc-rpc-username"), config.getString("epsmi.btc-rpc-password"))
    val coreConfig = Api4ElectrumCoreConfig(
      config.getBoolean("epsmi.enable-mempool-fee-histogram"),
      config.getString("epsmi.broadcast-method") match {
        case "own-node" => OwnNode
        case _ => UnsupportedBroadcastMethod
      },
      config.getInt(s"epsmi.server-port${if (isTestnet) "-testnet" else ""}"),
      isTestnet,
      btcRpcUsername,
      btcRpcPassword
    )

    val bitcoinSConnector = BitcoinSConnector(isTestnet, btcRpcUsername, btcRpcPassword)

    val scriptPubKeysToMonitorResult = new Setup(bitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

    val transactionMonitor = TransactionMonitorFactory.create(bitcoinSConnector.rpcCli)

    if (scriptPubKeysToMonitorResult.importNeeded){
      ???
    } else {
      val monitorState = transactionMonitor.buildAddressHistory(
        scriptPubKeysToMonitorResult.spksToMonitor,
        scriptPubKeysToMonitorResult.wallets
      )
      val server = RpcServer.startServer(coreConfig.port, transactionMonitor, monitorState, coreConfig)

      println(s"server started on port ${coreConfig.port}")

      Thread.sleep(360000000L)

      server.stop()
    }

  }

  doMain

}
