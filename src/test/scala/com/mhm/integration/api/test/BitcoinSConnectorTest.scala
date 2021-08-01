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
package com.mhm.integration.api.test

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorState
import com.mhm.epsmi.dummyprotocol.DummyTransactionMonitor
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

import scala.concurrent.ExecutionContext.Implicits.global

object BitcoinSConnectorTest extends FlatSpec {

  val blockHeader =
    new Api4ElectrumImpl(
      Api4ElectrumCore(TestBitcoinSConnector.rpcCli),
      new DummyTransactionMonitor,
      TransactionMonitorState.createEmpty()
    ).blockchainBlockHeader(652221)

  blockHeader shouldBe "00004020e028cc7a4447b6edb4a70c9165b7e11e2908c50483aa000000000000000000005df5cc1d04d234630bd7db39936d231a5f1ebb877d53be3961d88d37ed1be12340c9825fde950e172ea7194d"

}
