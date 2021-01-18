package com.mhm.epsmi.base

import com.mhm.util.MerkleProofOps
import com.mhm.util.MerkleProofOps.ReadResult
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class MerkleProofOpsHelperTest extends FlatSpec {

  "readVarInt" should "read an integer smaller than 253 from a buffer" in {
    val buf = hex"AAABAC".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](3, 172)
  }

  "readVarInt" should "read 2 byte integer from a buffer" in {
    val buf = hex"AAABFD0101".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](5, 257)
  }

  "readVarInt" should "read 4 byte integer from a buffer" in {
    val buf = hex"AAABFE00010000".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](7, 256)
  }

  "readVarInt" should "read 8 byte long from a buffer" in {
    val buf = hex"AAABFF0002000000000000".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](11, 512)
  }

}
