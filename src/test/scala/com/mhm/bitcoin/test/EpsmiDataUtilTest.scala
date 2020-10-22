package com.mhm.bitcoin.test

import com.mhm.util.EpsmiDataUtil
import org.scalatest.Matchers.convertToAnyShouldWrapper
import org.scalatest.{FlatSpec, WordSpec}

class EpsmiDataUtilTest extends FlatSpec {

  "log2" should "calculate log of base 2 " in {
    val f = EpsmiDataUtil.log2 _
    f(-4) shouldBe 0
    f(0) shouldBe 0
    f(1) shouldBe 0
    f(2) shouldBe 1
    f(4) shouldBe 2
    f(6) shouldBe 2
    f(8) shouldBe 3
    f(9) shouldBe 3
    f(512) shouldBe 9
  }

  "intCeilLog2" should "calculate int ceiling of log of base 2 " in {
    val f = EpsmiDataUtil.log2 _
    f(-4) shouldBe 0
    f(0) shouldBe 0
    f(1) shouldBe 0
    f(2) shouldBe 1
    f(4) shouldBe 2
    f(6) shouldBe 2
    f(8) shouldBe 3
    f(9) shouldBe 3
    f(512) shouldBe 9
  }

}
