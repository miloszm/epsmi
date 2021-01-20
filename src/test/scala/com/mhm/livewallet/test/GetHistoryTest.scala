package com.mhm.livewallet.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class GetHistoryTest extends FlatSpec with ServiceFixture {

  "get history for and address" should "return history" in {
    val historyResult =
      service.blockchainScripthashGetHistory("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")
    historyResult.nonEmpty shouldBe true
  }

}
