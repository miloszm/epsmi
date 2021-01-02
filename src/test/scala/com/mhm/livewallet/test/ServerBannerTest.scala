package com.mhm.livewallet.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class ServerBannerTest extends FlatSpec with ServiceFixture {

  "server banner api" should "return server banner" in {
    service.serverBanner().isEmpty shouldBe false
  }

}
