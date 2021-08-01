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
