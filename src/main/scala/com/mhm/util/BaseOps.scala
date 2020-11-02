package com.mhm.util

import java.math.BigInteger

import javax.xml.bind.DatatypeConverter
import scodec.bits.ByteVector

import scala.annotation.tailrec

object BaseOps {

  def decode(s: String, base: Int): BigInt = {
    if (base == 256){
      val v = ByteVector(DatatypeConverter.parseHexBinary(s))
      decodeBytes(v)
    }
    else if (base == 58) {
      val v = ByteVector.fromValidBase58(s)
      decodeBytes(v)
    }
    else
      throw new IllegalArgumentException("unsupported base")
  }

  def decodeBytes(v: ByteVector): BigInt = {
    @tailrec
    def go(i: BigInt, v: ByteVector): BigInt = {
      if (v.isEmpty) i else go((i * 256) + BigInt(v.head.toInt & 0xff), v.tail)
    }
    go(BigInt(v.head.toInt & 0xff), v.tail)
  }

}
