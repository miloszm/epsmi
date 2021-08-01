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

import javax.xml.bind.DatatypeConverter
import scodec.bits.ByteVector

import scala.annotation.tailrec

object BaseOps {

  def byteToUnsignedInt(b: Byte): Int = b.toInt & 0xff

  def decodeBase256(s: String): BigInt = {
    val v = ByteVector(DatatypeConverter.parseHexBinary(s))
    decodeBytesBase256(v)
  }

  def decodeBase58(s: String): BigInt = {
    val v = ByteVector.fromValidBase58(s)
    decodeBytesBase256(v)
  }

  def decodeBytesBase256(v: ByteVector): BigInt = {
    @tailrec
    def go(i: BigInt, v: ByteVector): BigInt = {
      if (v.isEmpty) i else go((i * 256) + BigInt(v.head.toInt & 0xff), v.tail)
    }
    go(BigInt(v.head.toInt & 0xff), v.tail)
  }

  def decodeBytesBase16(v: ByteVector): BigInt = {
    @tailrec
    def go(i: BigInt, v: ByteVector): BigInt = {
      if (v.isEmpty) i else go((i * 16) + BigInt(v.head.toInt & 0xf), v.tail)
    }
    go(BigInt(v.head.toInt & 0xf), v.tail)
  }

  def encodeBase256(i: BigInt, minLength: Int = 0): ByteVector = {
    @tailrec
    def go(n: BigInt, acc: ByteVector): ByteVector = {
      if (n == 0) acc else go(n / 256, (n % 256).toByte +: acc)
    }
    val v = go(i, ByteVector.empty)
    if (v.length < minLength) ByteVector.fill(minLength - v.length)(0) ++ v else v
  }

  def encodeBase58(i: BigInt): String = {
    ByteVector(i.toByteArray).toBase58
  }

  def changebase256to58(v: ByteVector): String = {
    encodeBase58(decodeBytesBase256(v))
  }

  def changebase58to256(s: String): ByteVector = {
    encodeBase256(decodeBase58(s))
  }

  def hexDecode(hs: String): Array[Byte] = {
    DatatypeConverter.parseHexBinary(hs)
  }

  def hexDecodeBV(hs: String): ByteVector = {
    ByteVector.fromHex(hs).getOrElse(throw new IllegalArgumentException("incorrect hex string"))
  }

  def hexEncode(b: Array[Byte]): String = {
    DatatypeConverter.printHexBinary(b)
  }

  def hexEncodeBV(b: ByteVector): String = {
    b.toHex.toUpperCase
  }

  def hexDecodeRev(hs: String): Array[Byte] = {
    DatatypeConverter.parseHexBinary(hs).reverse
  }

  def hexDecodeRevBV(hs: String): ByteVector = {
    hexDecodeBV(hs).reverse
  }

  def hexEncodeRev(b: Array[Byte]): String = {
    DatatypeConverter.printHexBinary(b.reverse)
  }

  def hexEncodeRevBV(b: ByteVector): String = {
    hexEncodeBV(b.reverse)
  }

}
