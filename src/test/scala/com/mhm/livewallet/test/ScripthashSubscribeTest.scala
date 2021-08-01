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

import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.bitcoins.core.protocol.BitcoinAddress
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class ScripthashSubscribeTest extends FlatSpec with ServiceFixture {

  "scripthash subscribe" should "return subscribe result" in {
    val subscribeResult =
      service.blockchainScripthashSubcribe("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    service.currentMonitorState.get.addressHistory.m.keys should contain(
      "5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2"
    )
    subscribeResult.length shouldBe 64
    subscribeResult shouldBe "6a7d8a2f84342ae7e3cca103ec43672b7185895e84e5d3ec8a17a5bc98692ada"
  }

  "valiate address" should "return hex starting with 19" in {

    /**
      * note - prefix "19" in the below is a problem - we need to remove it in a place where we use validateAddress - deterministic wallet
      * see note there, DeterministicWallet.getAddresses, ca. line 30
      */
    val validationResult =
      TestBitcoinSConnector.rpcCli.validateAddress(BitcoinAddress("12tohASdGUCDFvqaygaGbL7Jub7CiHdwa4"))
    wrap(validationResult).scriptPubKey
      .map(_.hex)
      .getOrElse(fail) shouldBe "1976a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac"
  }
}
