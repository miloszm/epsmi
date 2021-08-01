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

import com.mhm.util.BaseOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class BaseOpsTest extends FlatSpec {
  "decodeBase58" should "convert base58 encoded number into an integer" in {
    val decoded = BaseOps.decodeBase58(
      "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
    )
    decoded.toString shouldBe "4916168930508585312200884129123615957414675087408968371286219776133926492523251012231716720806962455048291126486067511577439222578945229342172280912299270752776086954255181781808200627932716490095"
  }

  "decodeBase256" should "convert 256-encoded string into an integer" in {
    val decoded = BaseOps.decodeBase256("d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec")
    decoded.toString shouldBe "97702535613963802964819910544120887115295631296599048914567293081941526643692"
  }

  "decodeBytesBase256" should "convert byte array into an integer" in {
    val decoded = BaseOps.decodeBytesBase256(hex"d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec")
    decoded.toString shouldBe "97702535613963802964819910544120887115295631296599048914567293081941526643692"
  }

  "decodeBytesBase16" should "convert byte array containing hex numbers into an integer" in {
    val decoded = BaseOps.decodeBytesBase16(hex"0a0b")
    decoded shouldBe BigInt(171)
  }

  "encode" should "convert big integer into byte array" in {
    val encoded =
      BaseOps.encodeBase256(BigInt("97702535613963802964819910544120887115295631296599048914567293081941526643692", 10))
    encoded.toHex shouldBe "d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec"
  }

  "encode" should "convert big integer into byte array with padding" in {
    val encoded = BaseOps.encodeBase256(BigInt(0), 4)
    encoded.toHex shouldBe "00000000"
  }

  "encode" should "convert big integer into a base58 encoded number" in {
    val encoded = BaseOps.encodeBase58(
      BigInt(
        "4916168930508585312200884129123615957414675087408968371286219776133926492523251012231716720806962455048291126486067511577439222578945229342172280912299270752776086954255181781808200627932716490095",
        10
      )
    )
    encoded shouldBe "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
  }

  "changebase256to58" should "convert bytes into base 58" in {
    val result = BaseOps.changebase256to58(
      hex"0488b21e000000000000000000d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37761870bb"
    )
    result shouldBe "xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ"
  }

  "changebase58to256" should "convert base 58 string into bytes" in {
    val result = BaseOps.changebase58to256(
      "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
    )
    result shouldBe hex"043587cf000000000000000000d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37a4565d6f"
  }

}
