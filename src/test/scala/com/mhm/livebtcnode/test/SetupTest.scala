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
package com.mhm.livebtcnode.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.main.SpksToMonitorFinder
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val result = new SpksToMonitorFinder(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
    result.spksToMonitor.nonEmpty shouldBe true
    result.wallets.nonEmpty shouldBe true
    println(s"getScriptPubKeysToMonitor result:")
    println(s"${result.spksToMonitor.size} spks to monitor, head is ${result.spksToMonitor.head}")
    println(s"wallets=${result.wallets}")
  }

}
