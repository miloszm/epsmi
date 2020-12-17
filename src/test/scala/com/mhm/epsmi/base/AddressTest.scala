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
    bitcoinAddress.scriptPubKey shouldBe ScriptPubKey.fromAsmBytes(hex"76a914e3106395976e56caf298de8ec6da3531378f2d5d88ac")
  }

  "conversion from bitcoin address to BitcoinAddress" should "work for testnet address" in {
    val bitcoinAddress = BitcoinAddress.fromString("mihBbdmqPut61bs9eDYZ3fdYxAKEP3mdiX")
    bitcoinAddress.value shouldBe "mihBbdmqPut61bs9eDYZ3fdYxAKEP3mdiX"
    bitcoinAddress.scriptPubKey shouldBe ScriptPubKey.fromAsmBytes(hex"76a91422d7a450712773509a8c3168f826c152cf07418488ac")
  }

}
