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

import com.mhm.common.model.HexHeight
import com.mhm.util.HashOps
import javax.xml.bind.DatatypeConverter
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class HashOpsTest extends FlatSpec {

  "hashMerkleRoot" should "produce valid hashed merkle root" in {
    val merkle = Array(
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
    val targetHash       = "5ce117fa1878fabc0d4c3153dad4e904593fc80c31aec6ebf4b3b5106f12c8d2"
    val pos              = 5
    val hashedMerkleRoot = HashOps.hashMerkleRoot(merkle, targetHash, pos)
    hashedMerkleRoot shouldBe "7AA87286736257006137F68298068B06139FFECDDBD7A1D6C25356F2336D13A7"
  }

  "sha256" should "produce valid hash" in {
    val in =
      "A9F44FFFEB404F994D4D0BB5EA924CFF7521FCFAE4D8F9874348A7440A1E1A15D2C8126F10B5B3F4EBC6AE310CC83F5904E9D4DA53314C0DBCFA7818FA17E15C"
    val hashed = HashOps.sha256(DatatypeConverter.parseHexBinary(in))
    DatatypeConverter.printHexBinary(hashed) shouldBe "C8330B9DAC36A8ACAC871647D809DDCEE1180D113207C7BB54E75593ABDBE5B5"
  }

  "sha256BV" should "produce valid hash" in {
    val in =
      hex"A9F44FFFEB404F994D4D0BB5EA924CFF7521FCFAE4D8F9874348A7440A1E1A15D2C8126F10B5B3F4EBC6AE310CC83F5904E9D4DA53314C0DBCFA7818FA17E15C"
    val hashed = HashOps.sha256BV(in)
    hashed.toHex shouldBe "c8330b9dac36a8acac871647d809ddcee1180d113207c7bb54e75593abdbe5b5"
  }

  "doHash" should "produce valid double hash" in {
    val in =
      "A9F44FFFEB404F994D4D0BB5EA924CFF7521FCFAE4D8F9874348A7440A1E1A15D2C8126F10B5B3F4EBC6AE310CC83F5904E9D4DA53314C0DBCFA7818FA17E15C"
    val doubleHashed = HashOps.doHash(DatatypeConverter.parseHexBinary(in))
    DatatypeConverter.printHexBinary(doubleHashed) shouldBe "3365A6D9582C22E89060433F8FF6AB11467071F6831E96FEC70D769EA0AA0DC0"
  }

  "doHashBV" should "produce valid double hash" in {
    val in =
      hex"A9F44FFFEB404F994D4D0BB5EA924CFF7521FCFAE4D8F9874348A7440A1E1A15D2C8126F10B5B3F4EBC6AE310CC83F5904E9D4DA53314C0DBCFA7818FA17E15C"
    val doubleHashed = HashOps.doHashBV(in)
    doubleHashed.toHex shouldBe "3365a6d9582c22e89060433f8ff6ab11467071f6831e96fec70d769ea0aa0dc0"
  }

  "binDblSha256" should "produce valid double hash" in {
    val dblSha = HashOps.binDblSha256(
      hex"043587cf000000000000000000d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37"
    )
    dblSha shouldBe hex"a4565d6fee32837dcd44133ffa67b8373ef10ecc9e2a5eda5b5c3729acb5370c"
  }

  "script2ScriptHash" should "hash given script" in {
    val hashed = HashOps.script2ScriptHash("76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac")
    hashed shouldBe "5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2"
  }

  "getStatusElectrum" should "return correct hash" in {
    val hashHeight1 = HexHeight("0ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt", 500)
    val hashHeight2 = HexHeight("1ttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttttt", 500)
    val status      = HashOps.getStatusElectrum(List(hashHeight1, hashHeight2))
    status shouldBe "801fe9bd87c8f1c5282d6eaf7bc2c7f474eb78fa8ea3fd4c916bdafdae64954d"
  }

}
