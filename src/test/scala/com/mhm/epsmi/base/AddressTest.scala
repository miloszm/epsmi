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
package com.mhm.epsmi.base

import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class AddressTest extends FlatSpec {

  "conversion from bitcoin address to BitcoinAddress" should "work for mainnet address" in {
    val bitcoinAddress = BitcoinAddress.fromString("1Mhc2GrKTdy1sdp6piYuqdp1dKmEoMyb75")
    bitcoinAddress.value shouldBe "1Mhc2GrKTdy1sdp6piYuqdp1dKmEoMyb75"
    bitcoinAddress.scriptPubKey shouldBe ScriptPubKey.fromAsmBytes(
      hex"76a914e3106395976e56caf298de8ec6da3531378f2d5d88ac"
    )
  }

  "conversion from bitcoin address to BitcoinAddress" should "work for testnet address" in {
    val bitcoinAddress = BitcoinAddress.fromString("mihBbdmqPut61bs9eDYZ3fdYxAKEP3mdiX")
    bitcoinAddress.value shouldBe "mihBbdmqPut61bs9eDYZ3fdYxAKEP3mdiX"
    bitcoinAddress.scriptPubKey shouldBe ScriptPubKey.fromAsmBytes(
      hex"76a91422d7a450712773509a8c3168f826c152cf07418488ac"
    )
  }

}
