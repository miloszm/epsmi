package com.mhm.main

import com.mhm.api4electrum.Api4ElectrumCoreConfig
import com.mhm.bitcoin.{AddressImporter, TransactionMonitorFactory}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory
import grizzled.slf4j.Logging

object Main extends App with Logging {
  def doMain(): Unit = {
    val config = ConfigFactory.load()
    val coreConfig = Api4ElectrumCoreConfig.init(config)

    val bitcoinSConnector = BitcoinSConnector(coreConfig.isTestnet, coreConfig.btcRpcUsername, coreConfig.btcRpcPassword)
    val SpksToMonitorResult(importNeeded, spksToMonitor, wallets) = new SpksToMonitorFinder(bitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

    if (importNeeded){
      AddressImporter.importAddresses(
        rpcCli = bitcoinSConnector.rpcCli,
        watchonlyAddresses = spksToMonitor,
        wallets = wallets,
        changeParam = -1,
        count = coreConfig.initialImportCount
      )
      logger.info(
        "Import done. " +
        "\nIf recovering a wallet which already has existing transactions, then" +
        "\nrun the rescan script (NOT IMPLEMENTED YET)." +
        "\nIf you're confident that the wallets are new and empty" +
        "\nthen there's no need to rescan, just restart this script."
      )
    } else {
      val transactionMonitor = TransactionMonitorFactory.create(bitcoinSConnector.rpcCli)
      val monitorState = transactionMonitor.buildAddressHistory(spksToMonitor, wallets)
      val server = RpcServer.startServer(transactionMonitor, monitorState, coreConfig)
      logger.info(s"server started on port ${coreConfig.port}")
      Thread.sleep(31536000000L)
      server.stop()
    }
  }

  doMain
}
