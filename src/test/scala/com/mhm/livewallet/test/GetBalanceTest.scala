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
