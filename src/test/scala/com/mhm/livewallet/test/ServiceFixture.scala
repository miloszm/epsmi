/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.livewallet.test

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorFactory
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.ec
import com.mhm.main.SpksToMonitorFinder
import com.typesafe.config.ConfigFactory

trait ServiceFixture {

  private val config             = ConfigFactory.load()
  private val spksToMonitor      = new SpksToMonitorFinder(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
  private val transactionMonitor = TransactionMonitorFactory.create(TestBitcoinSConnector.rpcCli)
  private val monitorState       = transactionMonitor.buildAddressHistory(spksToMonitor.spksToMonitor, spksToMonitor.wallets)
  val service                    = new Api4ElectrumImpl(Api4ElectrumCore(TestBitcoinSConnector.rpcCli), transactionMonitor, monitorState)

}
