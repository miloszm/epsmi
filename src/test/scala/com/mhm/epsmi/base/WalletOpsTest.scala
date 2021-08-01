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

import com.mhm.wallet.WalletOps
import com.mhm.wallet.WalletOps.XKeyRawTuple
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits._

class WalletOpsTest extends FlatSpec {

  "convertToStandardXpub" should "return standard xpub" in {
    val mpk =
      "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
    val xpub = WalletOps.convertToStandardXpub(mpk, hex"0488b21e")
    xpub shouldBe "xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ"
  }

  "bip32Serialize for extended key" should "return serialized extended key given elements as in BIP32 serialization format" in {
    val xKeyRawTuple = XKeyRawTuple(
      hex"0488b21e",
      0,
      hex"00000000",
      0,
      hex"d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec",
      hex"026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37"
    )
    val serXKey = WalletOps.bip32Serialize(xKeyRawTuple)
    serXKey shouldBe "xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ"
  }

  "bip32Deserialize for extended key" should "return elements as in BIP32 serialization format" in {
    val xKeyRawTuple = WalletOps.bip32Deserialize(
      "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
    )
    xKeyRawTuple.vbytes shouldBe hex"043587cf"
    xKeyRawTuple.i shouldBe 0
    xKeyRawTuple.fingerprint shouldBe hex"00000000"
    xKeyRawTuple.depth shouldBe 0
    xKeyRawTuple.chaincode shouldBe hex"d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec"
    xKeyRawTuple.key shouldBe hex"026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37"
  }

}
