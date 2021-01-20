package com.mhm.livewallet.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class GetBalanceTest extends FlatSpec with ServiceFixture {

  "get balance on an address" should "return balance result" in {
    val balanceResult =
      service.blockchainScripthashGetBalance("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    balanceResult.confirmed > 0 shouldBe true
  }

}
