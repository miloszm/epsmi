package com.mhm.livewallet.test

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.connectors.BitcoinSConnector
import com.mhm.main.Setup
import com.typesafe.config.ConfigFactory

trait ServiceFixture {

  private val config = ConfigFactory.load()
  private val spksToMonitor = new Setup(BitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
  private val transactionMonitor = TransactionMonitorFactory.create(BitcoinSConnector.rpcCli)
  private val monitorState = transactionMonitor.buildAddressHistory(spksToMonitor.spksToMonitor, spksToMonitor.wallets)
  val service = new Api4ElectrumImpl(Api4ElectrumCore(BitcoinSConnector.rpcCli), transactionMonitor, monitorState)

}
