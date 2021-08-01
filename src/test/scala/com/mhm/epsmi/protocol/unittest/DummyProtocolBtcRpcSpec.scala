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
package com.mhm.epsmi.protocol.unittest

import com.mhm.api4electrum.Api4ElectrumCore
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.dummyprotocol.DummyProtocolBtcRpc
import com.mhm.epsmi.dummyprotocol.DummyProtocolBtcRpc.DummyJsonrpcBlockchainHeight
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.ec
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class DummyProtocolBtcRpcSpec extends FlatSpec {

  "DummyBtcProtocolRpc" should "support GetBlockHeader" in {
    val rpc = DummyProtocolBtcRpc()
    for (height <- 0 until 1000) {
      val blockhash = wrap(rpc.getBlockHash(height))
      val hexHeight = wrap(Api4ElectrumCore(rpc).getBlockHeaderRaw(blockhash))
      hexHeight.hex.length shouldBe 160
      val header = wrap(Api4ElectrumCore(rpc).getBlockHeaderCooked(blockhash))
      header.version shouldBe 536870912
    }
  }

  "DummyBtcProtocolRpc" should "support GetCurrentHeader" in {
    val rpc                         = DummyProtocolBtcRpc()
    val (bestBlockHash1, hexHeight) = wrap(Api4ElectrumCore(rpc).getCurrentHeaderRaw())
    hexHeight.hex.length shouldBe 160
    bestBlockHash1.nonEmpty shouldBe true
    val (bestBlockHash2, header) = wrap(Api4ElectrumCore(rpc).getCurrentHeaderCooked())
    header.version shouldBe 536870912
    bestBlockHash2.nonEmpty shouldBe true
  }

  "DummyBtcProtocolRpc" should "support GetBlockHeadersHex" in {
    val rpc = DummyProtocolBtcRpc()
    for (startHeight <- Seq(100, DummyJsonrpcBlockchainHeight + 10, DummyJsonrpcBlockchainHeight - 10, 0))
      for (count <- Seq(200, 5, 15, 250)) {
        val result          = wrap(Api4ElectrumCore(rpc).getBlockHeaders(startHeight, count))
        val availableBlocks = -Math.min(0, startHeight - DummyJsonrpcBlockchainHeight - 1)
        val expectedCount   = Math.min(availableBlocks, count)
        result.hex.length shouldBe expectedCount * 80 * 2 //#80 bytes/header, 2 chars/byte
        result.count shouldBe expectedCount
      }
  }

}
