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

import com.mhm.util.{HashOps, MerkleProofOps}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper, have}

class MerkleProofOpsTest extends FlatSpec {

  "convertCoreToElectrumMerkleProof" should "work for case 1" in {
    val merkleBlockHex =
      "00e0ff37ebe3f41b516fd406f55046c0b5783e182e2e634b33e70d000000000000000000a7136d33f25653c2d6a1d7dbcdfe9f13068b069882f63761005762738672a87a7d45875fde950e176c822606840800000dc37acd5f8d7273f3ca89561ef4d689c42c29efc23ada7ff6ccb21e8d6e0c6689a9f44fffeb404f994d4d0bb5ea924cff7521fcfae4d8f9874348a7440a1e1a15d2c8126f10b5b3f4ebc6ae310cc83f5904e9d4da53314c0dbcfa7818fa17e15c4bacb199a8eb2902c20ba06d433accea1dc0b2796c083091217db99bf43feb7f583340dd7c96c86f37fc97956ab647fd673f10ac1c5d1c51dedc486287d730029664cafa640db00c2457ba7ec6079753ef35a99adaf247fb0c20c9750823979e992e6f119a1ae3bbebf769d33dba4f0c2b1a79294d99bc9c1f03ac02b276caee1599cdebfe09fa00dd5ddc185cbb1fa62ecbcf518e2d78f36bf00c1a516845116db844d4f3bbfc8814abeb972faf87c7e368626d24e0c1c0a6373d89c03b2d7c878df412bf3772190d401cc338c981724fcf99b6ada3b8210b4152ded6c20a171729b8df3a470fc64c6a029e7b991617818d63b7f862f107293a6dea24ae6c2857a351ab859509439df573274edde70120fcef57fa3c3dde47a959605f3dec55109f582cb9cfdee5cbe2c5102044b546a758d50fb8bfd31cd83f1803c93d6a1204ff5b0000"
    val electrumMerkleProof = MerkleProofOps.convertCoreToElectrumMerkleProof(merkleBlockHex)
    electrumMerkleProof.pos shouldBe 5
    electrumMerkleProof.txId shouldBe "5CE117FA1878FABC0D4C3153DAD4E904593FC80C31AEC6EBF4B3B5106F12C8D2"
    electrumMerkleProof.merkleRoot shouldBe "7AA87286736257006137F68298068B06139FFECDDBD7A1D6C25356F2336D13A7"
    electrumMerkleProof.merkle.length shouldBe 12
    (electrumMerkleProof.merkle should contain).theSameElementsAs(
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
  }

  "convertCoreToElectrumMerkleProof" should "work also for case 2" in {
    val merkleBlockHex =
      "00e0002061885fda5712581ecc5fa425d40c41a2dea67c790f722f460c000000000000003a5cb203ed0bd201f55288a510f60e60750e830b5e00d0ad19e471f26afcc93f3582de5f51d9461925e2adc65500000007e54f1ee8024b5620c4a472ad7ff47d7373c94727b2f8e0528b71e89db6171cc2ee3cba46181cc8ed03dca66acdbeec4f58cde28ded8e0bb280a81340a0d48d02258bf173eb974611d781e112fb1bc6fb642a869f31bdcc7caf255cf7002e0fab0ed2c773546d01039de01d5cb077c9234cb4b91cda70c87906cf98a48a077d4212bb189f8814b67588bf960c889849c3bd8e17377ce773cc0f911bcaf4de962c34f6f27d830f242c9c2591b3b275b4a299dabbc85edcb5cf0c8328d2dbae48c1d0fa483c39bd3ca738de12c5ad6de840b8f31bfea061fc50ccf39aabf43d413d02bd06"
    val electrumMerkleProof = MerkleProofOps.convertCoreToElectrumMerkleProof(merkleBlockHex)
    electrumMerkleProof.txId shouldBe "427d078aa498cf0679c870da1cb9b44c23c977b05c1de09d03016d5473c7d20e".toUpperCase
    electrumMerkleProof.merkleRoot shouldBe "3FC9FC6AF271E419ADD0005E0B830E75600EF610A58852F501D20BED03B25C3A"
    println("input to hashMerkleRoot:")
    println(s"merkle=${electrumMerkleProof.merkle.mkString("|")}")
    println(s"txId=${electrumMerkleProof.txId}")
    println(s"pos=${electrumMerkleProof.pos}")

    val desiredElectrumMerkle = Array(
      "2c96def4ca1b910fcc73e77c37178ebdc34998880c96bf8875b614889f18bb12",
      "ab0f2e00f75c25af7cccbd319f862a64fbc61bfb12e181d7114697eb73f18b25",
      "028dd4a04013a880b20b8eed8de2cd584fecbecd6aa6dc03edc81c1846ba3cee",
      "c148aedbd228830ccfb5dc5ec8bbda99a2b475b2b391259c2c240f837df2f634",
      "3d413df4ab9af3cc50fc61a0fe1bf3b840e86dadc512de38a73cbd393c48fad0",
      "1baf786713f7edc6d84b4421ebcff92651df5988239e5eb8787874ea83e62d7d",
      "c21c17b69de8718b52e0f8b22747c973737df47fad72a4c420564b02e81e4fe5"
    ).map(_.toUpperCase)
    (electrumMerkleProof.merkle should contain).theSameElementsInOrderAs(desiredElectrumMerkle)

    val impliedMerkleRoot =
      HashOps.hashMerkleRoot(desiredElectrumMerkle, electrumMerkleProof.txId, electrumMerkleProof.pos)
    impliedMerkleRoot shouldBe electrumMerkleProof.merkleRoot
  }

  "deserializeCoreFormatMerkleProof" should "provide correct deserialization" in {
    val hashList = Array(
      "89660C6E8D1EB2CCF67FDA3AC2EF292CC489D6F41E5689CAF373728D5FCD7AC3",
      "151A1E0A44A7484387F9D8E4FAFC2175FF4C92EAB50B4D4D994F40EBFF4FF4A9",
      "5CE117FA1878FABC0D4C3153DAD4E904593FC80C31AEC6EBF4B3B5106F12C8D2",
      "7FEB3FF49BB97D219130086C79B2C01DEACC3A436DA00BC20229EBA899B1AC4B",
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
    val flagValue: Array[Byte] = Array(255.toByte, 91.toByte, 0, 0)
    val txCount                = 2180
    val merkleNode             = MerkleProofOps.deserializeCoreFormatMerkleProof(hashList, flagValue, txCount)
    merkleNode.toString shouldBe
      """(((((((((("89660C6E8D1EB2CCF67FDA3AC2EF292CC489D6F41E5689CAF373728D5FCD7AC3", (("151A1E0A44A7484387F9D8E4FAFC2175FF4C92EAB50B4D4D994F40EBFF4FF4A9", "tx:5:5CE117FA1878FABC0D4C3153DAD4E904593FC80C31AEC6EBF4B3B5106F12C8D2"), "7FEB3FF49BB97D219130086C79B2C01DEACC3A436DA00BC20229EBA899B1AC4B")), "0230D7876248DCDE511C5D1CAC103F67FD47B66A9597FC376FC8967CDD403358"), "9E97230875C9200CFB47F2DA9AA935EF539707C67EBA57240CB00D64FACA6496"), "EECA76B202AC031F9CBC994D29791A2B0C4FBA3DD369F7EBBBE31A9A116F2E99"), "114568511A0CF06BF3782D8E51CFCB2EA61FBB5C18DC5DDD00FA09FEEBCD9915"), "7C2D3BC0893D37A6C0C1E0246D6268E3C787AF2F97EBAB1488FCBBF3D444B86D"), "170AC2D6DE52410B21B8A3ADB699CF4F7281C938C31C400D197237BF12F48D87"), "286CAE24EA6D3A2907F162F8B7638D811716997B9E026A4CC60F473ADFB82917"), "55EC3D5F6059A947DE3D3CFA57EFFC2001E7DD4E2773F59D43099585AB51A357"), "126A3DC903183FD81CD3BFB80FD558A746B5442010C5E2CBE5DECFB92C589F10")"""
  }

}
