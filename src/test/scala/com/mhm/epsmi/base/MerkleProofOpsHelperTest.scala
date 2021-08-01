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

import com.mhm.util.MerkleProofOps
import com.mhm.util.MerkleProofOps.ReadResult
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class MerkleProofOpsHelperTest extends FlatSpec {

  "readVarInt" should "read an integer smaller than 253 from a buffer" in {
    val buf    = hex"AAABAC".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](3, 172)
  }

  "readVarInt" should "read 2 byte integer from a buffer" in {
    val buf    = hex"AAABFD0101".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](5, 257)
  }

  "readVarInt" should "read 4 byte integer from a buffer" in {
    val buf    = hex"AAABFE00010000".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](7, 256)
  }

  "readVarInt" should "read 8 byte long from a buffer" in {
    val buf    = hex"AAABFF0002000000000000".toArray
    val result = MerkleProofOps.readVarInt(buf, 2)
    result shouldBe ReadResult[Int](11, 512)
  }

}
