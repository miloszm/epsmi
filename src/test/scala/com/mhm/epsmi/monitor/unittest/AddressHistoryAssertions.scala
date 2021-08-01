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
package com.mhm.epsmi.monitor.unittest

import com.mhm.common.model.AddressHistory
import com.mhm.util.HashOps.script2ScriptHash
import org.scalatest.Matchers

trait AddressHistoryAssertions extends Matchers {

  def assertAddressHistoryTx(addressHistory: AddressHistory,
                             spk: String,
                             height: Int,
                             txId: String,
                             subscribed: Boolean): Unit = {
    val historyEntry = addressHistory.m.getOrElse(script2ScriptHash(spk), fail)
    historyEntry.history.head.height shouldBe height
    historyEntry.history.head.txHash shouldBe txId
//    if (height == 0)
//      historyEntry.history.head.fee shouldBe 0 // TODO
    historyEntry.subscribed shouldBe subscribed
  }

  def assertHistoryEmpty(addressHistory: AddressHistory, spk: String): Unit = {
    addressHistory.m.getOrElse(script2ScriptHash(spk), fail).history.size shouldBe 0
  }

}
