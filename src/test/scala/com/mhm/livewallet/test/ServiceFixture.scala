package com.mhm.livewallet.test

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.ec
import com.mhm.main.SpksToMonitorFinder
import com.typesafe.config.ConfigFactory

trait ServiceFixture {

  private val config = ConfigFactory.load()
  private val spksToMonitor = new SpksToMonitorFinder(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
  private val transactionMonitor = TransactionMonitorFactory.create(TestBitcoinSConnector.rpcCli)
  private val monitorState = transactionMonitor.buildAddressHistory(spksToMonitor.spksToMonitor, spksToMonitor.wallets)
  val service = new Api4ElectrumImpl(Api4ElectrumCore(TestBitcoinSConnector.rpcCli), transactionMonitor, monitorState)

}
