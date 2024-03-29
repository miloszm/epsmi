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
package com.mhm.util

import java.security.MessageDigest

import com.mhm.common.model.HexHeight
import com.mhm.util.BaseOps.{hexDecodeRevBV, hexEncodeRevBV}
import javax.xml.bind.DatatypeConverter.printHexBinary
import scodec.bits.ByteVector

object HashOps {

  def hashMerkleRoot(merkle: Array[String], targetHash: String, pos: Int): String = {
    var h = hexDecodeRevBV(targetHash)
    for (i <- merkle.indices) {
      val item = merkle(i)
      h =
        if (((pos >> i) & 1) != 0)
          doHashBV(hexDecodeRevBV(item) ++ h)
        else
          doHashBV(h ++ hexDecodeRevBV(item))
    }
    hexEncodeRevBV(h)
  }

  def sha256(a: Array[Byte]): Array[Byte] = {
    MessageDigest.getInstance("SHA-256").digest(a)
  }

  def sha256BV(a: ByteVector): ByteVector = {
    ByteVector(sha256(a.toArray))
  }

  def doHash(a: Array[Byte]): Array[Byte] = {
    sha256(sha256(a))
  }

  def doHashBV(a: ByteVector): ByteVector = {
    sha256BV(sha256BV(a))
  }

  def binDblSha256(v: ByteVector): ByteVector = {
    doHashBV(v)
  }

  def script2ScriptHash(s: String): String = {
    sha256BV(BaseOps.hexDecodeBV(s)).take(32).reverse.toHex
  }

//  # 'result' field in the blockchain.scripthash.subscribe method
//  # reply uses this as a summary of the address
  def getStatusElectrum(h: List[HexHeight]): String = {
    if (h.isEmpty) ""
    else {
      val status = h.map(hh => s"${hh.hex}:${hh.height}:").mkString
      printHexBinary(sha256(status.getBytes)).toLowerCase
    }
  }

}
