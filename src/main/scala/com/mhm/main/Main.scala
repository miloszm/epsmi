package com.mhm.main

import com.mhm.api4electrum.Api4ElectrumCoreConfig
import com.mhm.bitcoin.{AddressImporter, OwnNode, TransactionMonitorFactory, UnsupportedBroadcastMethod}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory
import grizzled.slf4j.Logging

object Main extends App with Logging {
  def doMain(): Unit = {
    val config = ConfigFactory.load()
    val isTestnet = config.getBoolean("epsmi.testnet")
    val initialImportCount = config.getInt("epsmi.initial-import-count")
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
      AddressImporter.importAddresses(
        rpcCli = bitcoinSConnector.rpcCli,
        watchonlyAddresses = scriptPubKeysToMonitorResult.spksToMonitor,
        wallets = scriptPubKeysToMonitorResult.wallets,
        changeParam = -1,
        count = initialImportCount
      )
      logger.info("Import done. \nIf recovering a wallet which already has existing" +
        " transactions, then\nrun the rescan script (NOT IMPLEMENTED YET). If you're confident" +
        " that the wallets are new\nand empty then there's no need to" +
        " rescan, just restart this script")
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
