package com.mhm.bitcoin.integration.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class TrIdFromPosMerkleFalseTest extends FlatSpec with IntTestFixture {

  "tr id from pos api with merkle param set to false" should "return correct transaction hex" in {
    val transactionId = fixture.client.blockchainTrIdFromPos(652742, 5, false)
    transactionId shouldBe "5ce117fa1878fabc0d4c3153dad4e904593fc80c31aec6ebf4b3b5106f12c8d2"

    println(s"trIdFromPos = $transactionId")
    val trHex = fixture.client.blockchainTransactionGet(transactionId)
    println(s"trHex for the above = $trHex")
  }

}
