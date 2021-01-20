package com.mhm.main

import com.mhm.api4electrum.Api4ElectrumCoreConfig
import com.mhm.bitcoin.{
  AddressImporter,
  NoopTxsMonitorStateListener,
  NoopWalletStateListener,
  ReScanner,
  RescanConfig,
  TransactionMonitorFactory,
  TxsMonitor,
  TxsMonitorStateListener,
  WalletStateListener
}
import com.mhm.connectors.BitcoinSConnector
import com.mhm.rpcserver.RpcServer
import com.typesafe.config.ConfigFactory
import grizzled.slf4j.Logging

import scala.sys.exit

object Main extends App with Logging {

  def initilizeJmx(): TxsMonitorStateListener with WalletStateListener = {
    import javax.management.InstanceAlreadyExistsException
    import javax.management.MBeanRegistrationException
    import javax.management.MalformedObjectNameException
    import javax.management.NotCompliantMBeanException
    import javax.management.ObjectName
    import java.lang.management.ManagementFactory
    val txsMonitor = new TxsMonitor()
    try {
      val objectName = new ObjectName("com.mhm.epsmi:type=current,name=txsmonitor")
      val server     = ManagementFactory.getPlatformMBeanServer
      server.registerMBean(txsMonitor, objectName)
    } catch {
      case e @ (_: MalformedObjectNameException | _: InstanceAlreadyExistsException | _: MBeanRegistrationException |
          _: NotCompliantMBeanException) =>
        logger.error("jmx initialization error", e)
        throw e
    }
    txsMonitor
  }

  def doMain(): Unit = {
    val config       = ConfigFactory.load()
    val coreConfig   = Api4ElectrumCoreConfig.init(config)
    val rescanConfig = RescanConfig.init(config)

    val bitcoinSConnector =
      BitcoinSConnector(coreConfig.isTestnet, coreConfig.btcRpcUsername, coreConfig.btcRpcPassword)

    if (rescanConfig.rescan) {
      ReScanner.rescan(bitcoinSConnector.rpcCli, rescanConfig.startBlock)
    } else {
      val txsMonitorOpt = if (config.getBoolean("epsmi.jmx")) Some(initilizeJmx()) else None
      val SpksToMonitorResult(importNeeded, spksToMonitor, wallets) =
        new SpksToMonitorFinder(bitcoinSConnector.rpcCli, config)
          .getScriptPubKeysToMonitor(txsMonitorOpt.getOrElse(NoopWalletStateListener))
      if (importNeeded) {
        AddressImporter.importAddresses(
          rpcCli             = bitcoinSConnector.rpcCli,
          watchonlyAddresses = spksToMonitor,
          wallets            = wallets,
          changeParam        = -1,
          count              = coreConfig.initialImportCount
        )
        logger.info(
          "Import done. " +
            "\nIf recovering a wallet which already has existing transactions, then" +
            "\nrun the server with rescan=true set in configuration." +
            "\nIf you're confident that the wallets are new and empty" +
            "\nthen there's no need to rescan, just restart this script."
        )
      } else {
        val transactionMonitor = TransactionMonitorFactory.create(bitcoinSConnector.rpcCli)
        val monitorState       = transactionMonitor.buildAddressHistory(spksToMonitor, wallets)
        val server = RpcServer.startServer(
          transactionMonitor,
          monitorState,
          coreConfig,
          txsMonitorOpt.getOrElse(NoopTxsMonitorStateListener)
        )
        logger.info(s"server started on port ${coreConfig.port}")
        Thread.sleep(31536000000L)
        server.stop()
      }
    }
  }

  try {
    doMain
  } catch {
    case e: Throwable =>
      logger.error(s"exception caught in main: ${e.getClass.getName}", e)
      logger.info(s"${Constants.SERVER_NAME} exiting...")
      exit(1)
  } finally {
    logger.info(s"${Constants.SERVER_NAME} terminating...")
    exit(0)
  }
}
