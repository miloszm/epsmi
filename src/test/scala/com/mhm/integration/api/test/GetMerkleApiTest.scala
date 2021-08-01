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
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class GetMerkleApiTest extends FlatSpec with IntTestFixture {
  "get merkle api" should "return correct merkle" in {

    /**
      * we don't want to make test dependent on the wallet content,
      * so we search transaction first for it to be found by get merkle
      */
    val txId4GetMerkle = fixture.client.blockchainTrIdFromPos(652742, 5, false)
    val merkle         = fixture.client.blockchainTransactionGetMerkle(txId4GetMerkle, 0)

    merkle.block_height shouldBe 652742
    merkle.pos shouldBe 5

    (merkle.merkle should contain).theSameElementsAs(
      List(
        "151A1E0A44A7484387F9D8E4FAFC2175FF4C92EAB50B4D4D994F40EBFF4FF4A9",
        "7FEB3FF49BB97D219130086C79B2C01DEACC3A436DA00BC20229EBA899B1AC4B",
        "89660C6E8D1EB2CCF67FDA3AC2EF292CC489D6F41E5689CAF373728D5FCD7AC3",
        "0230D7876248DCDE511C5D1CAC103F67FD47B66A9597FC376FC8967CDD403358",
        "9E97230875C9200CFB47F2DA9AA935EF539707C67EBA57240CB00D64FACA6496",
        "EECA76B202AC031F9CBC994D29791A2B0C4FBA3DD369F7EBBBE31A9A116F2E99",
        "114568511A0CF06BF3782D8E51CFCB2EA61FBB5C18DC5DDD00FA09FEEBCD9915",
        "7C2D3BC0893D37A6C0C1E0246D6268E3C787AF2F97EBAB1488FCBBF3D444B86D",
        "170AC2D6DE52410B21B8A3ADB699CF4F7281C938C31C400D197237BF12F48D87",
        "286CAE24EA6D3A2907F162F8B7638D811716997B9E026A4CC60F473ADFB82917",
        "55EC3D5F6059A947DE3D3CFA57EFFC2001E7DD4E2773F59D43099585AB51A357",
        "126A3DC903183FD81CD3BFB80FD558A746B5442010C5E2CBE5DECFB92C589F10"
      )
    )

    println(s"get merkle result = ")
    println(s"   blockHeight=${merkle.block_height}")
    println(s"   pos=${merkle.pos}")
    println(s"   merkle=")
    merkle.merkle.foreach { println(_) }

  }

}
