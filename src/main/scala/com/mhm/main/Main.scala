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

    val scriptPubKeysToMonitorResult = new Setup(bitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()

    val transactionMonitor = TransactionMonitorFactory.create(bitcoinSConnector.rpcCli)

    if (scriptPubKeysToMonitorResult.importNeeded){
      AddressImporter.importAddresses(
        rpcCli = bitcoinSConnector.rpcCli,
        watchonlyAddresses = scriptPubKeysToMonitorResult.spksToMonitor,
        wallets = scriptPubKeysToMonitorResult.wallets,
        changeParam = -1,
        count = coreConfig.initialImportCount
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
