package com.mhm.util

import java.nio.ByteBuffer

import org.bitcoins.core.number.UInt32
import scodec.bits.ByteVector

object EpsmiDataOps {
  def reverse(array: Array[Byte]): Array[Byte] = {
    val l = array.length
    val a = Array.ofDim[Byte](l)
    for (i <- a.indices) a(i) = array(l - 1 - i)
    a
  }

  def byteVectorOrZeroToArray(bvOpt: Option[ByteVector], len: Int): Array[Byte] = {
    bvOpt.fold(Array.fill[Byte](len)(0))(byteVectorToArray)
  }

  def byteVectorToArray(bv: ByteVector): Array[Byte] = {
    val a = Array.ofDim[Byte](bv.length.toInt)
    bv.copyToArray(a, 0)
    reverse(a)
  }

  def intToArray(i: Int): Array[Byte] = {
    val b = ByteBuffer.allocate(4)
    b.putInt(i)
    reverse(b.array())
  }

  def uint32ToArray(i: UInt32): Array[Byte] = {
    val a = Array.ofDim[Byte](4)
    val b = ByteBuffer.allocate(4)
    i.bytes.copyToArray(a, 0)
    b.put(a)
    reverse(a)
  }

  def log2(x: Int): Int = {
    if (x <= 0) 0 else (Math.log(x) / Math.log(2)).toInt
  }

  def intCeilLog2(x: Int): Int = {
    if (x <= 0) 0 else
    Math.ceil(Math.log(x) / Math.log(2)).toInt
  }


}
