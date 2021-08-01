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
package com.mhm.integration.api.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class TrIdFromPosMerkleFalseTest extends FlatSpec with IntTestFixture {

  "tr id from pos api with merkle param set to false" should "return correct transaction hex" in {
    val transactionId = fixture.client.blockchainTrIdFromPos(652742, 5, false)
    transactionId shouldBe "5ce117fa1878fabc0d4c3153dad4e904593fc80c31aec6ebf4b3b5106f12c8d2"
    val trHex = fixture.client.blockchainTransactionGet(transactionId)
    trHex shouldBe "02000000000101b85f0589c07c79927432d0d48247800ac6147f16d5d7f6b73135be945fbd1672010000001716001421d58e56d047c7ba486d19db9453bace721066eafdffffff02d46876010000000017a914ea3577cbbf19225199ee6546282ba3b50d39735f87b84b1f000000000017a9142ed14c10d75f41250402f737dc225a5ece1a0c368702473044022032dd830e6a911bfeb5321ccaa1bb15bf107659d5454acc6d928df87a5c99f36b02203cbc006bac8630a53cff7cb479eec5018701fd45258ffbadbbecd478bf8be3fe012103b6791b33fd88d8ddd7d97a72c447b8a7d8deb74541526a5cdffd5510dc745cfec4f50900"
  }

}
