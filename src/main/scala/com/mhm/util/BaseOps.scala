package com.mhm.util

import javax.xml.bind.DatatypeConverter
import scodec.bits.ByteVector

import scala.annotation.tailrec

object BaseOps {

  def decodeBase256(s: String): BigInt = {
    val v = ByteVector(DatatypeConverter.parseHexBinary(s))
    decodeBytes(v)
  }

  def decodeBase58(s: String): BigInt = {
    val v = ByteVector.fromValidBase58(s)
    decodeBytes(v)
  }

  def decodeBytes(v: ByteVector): BigInt = {
    @tailrec
    def go(i: BigInt, v: ByteVector): BigInt = {
      if (v.isEmpty) i else go((i * 256) + BigInt(v.head.toInt & 0xff), v.tail)
    }
    go(BigInt(v.head.toInt & 0xff), v.tail)
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

}
