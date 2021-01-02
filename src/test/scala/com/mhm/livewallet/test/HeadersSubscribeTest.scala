package com.mhm.livewallet.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class HeadersSubscribeTest extends FlatSpec with ServiceFixture {

  "headers subscribe" should "return subscribe result" in {
    val subscribeResult = service.blockchainHeadersSubcribe()
    subscribeResult.height > 0 shouldBe true
    subscribeResult.hex.length shouldBe 160
  }

}
