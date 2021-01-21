package com.mhm.epsmi.base

import com.mhm.util.EpsmiDataOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class EpsmiDataOpsTest extends FlatSpec {

  "log2" should "calculate log of base 2 " in {
    val f = EpsmiDataOps.log2 _
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
    val f = EpsmiDataOps.intCeilLog2 _
    f(-4) shouldBe 0
    f(0) shouldBe 0
    f(1) shouldBe 0
    f(2) shouldBe 1
    f(4) shouldBe 2
    f(6) shouldBe 3
    f(8) shouldBe 3
    f(9) shouldBe 4
    f(512) shouldBe 9
  }

}
