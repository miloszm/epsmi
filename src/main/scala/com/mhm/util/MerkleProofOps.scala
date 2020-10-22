package com.mhm.util

import java.nio.ByteBuffer

import com.mhm.api4electrum.Api4ElectrumCore.ElectrumMerkleProof
import com.mhm.util.EpsmiDataUtil.intCeilLog2
import javax.xml.bind.DatatypeConverter

import scala.collection.mutable.ArrayBuffer

object MerkleProofOps {
  /**
  def deserialize_core_format_merkle_proof(hash_list, flag_value, txcount):
  """Converts core's format for a merkle proof into a tree in memory"""
  tree_depth = int(ceil(log(txcount, 2)))
  hashes = iter(hash_list)
  #one-liner which converts the flags value to a list of True/False bits
    flags = (flag_value[i//8]&1 << i%8 != 0 for i in range(len(flag_value)*8))
  try:
  root_node = decend_merkle_tree(hashes, flags, tree_depth, txcount, 0)
  return root_node
  except StopIteration:
    raise ValueError
   */
  def deserializeCoreFormatMerkleProof(hashList: Array[String], flagValue: Array[Byte], txCount: Int): Unit = {
    val treeDepth = intCeilLog2(txCount)
  }


  def convertCoreToElectrumMerkleProof(merkleBlockHex: String): ElectrumMerkleProof = {

    def readAsInt(buf: Array[Byte], pos: Int, bytez: Int): (Int, Int) = {
      val newPos = pos + bytez
      val intBytes = ByteBuffer.wrap(buf.slice(80, 84).reverse)
      (newPos, intBytes.getInt)
    }

    def readVarInt(buf: Array[Byte], pos: Int): (Int, Int) = {
      val v = buf(pos)
      if ( v < 253) (pos + 1, v) else {
        val newPos = pos + 1 + Math.pow(2.0, v-252).toInt // TODO this `else` part needs to be tested
        val intBytes = ByteBuffer.wrap(buf.slice(newPos, newPos+4).reverse)
        (newPos, intBytes.getInt)
      }
    }

    def readBytes(buf: Array[Byte], pos: Int, bytez: Int): (Int, Array[Byte]) = {
      val newPos = pos + bytez
      (newPos, buf.slice(newPos - bytez, newPos).reverse)
    }

    var pos = 80
    val proof = DatatypeConverter.parseHexBinary(merkleBlockHex)
    val merkleProof = ByteBuffer.wrap(proof.slice(36, 32))
    val (p1, txCount) = readAsInt(proof, pos, 4)
    pos = p1
    println(s"txCount=$txCount")

    val (p2, hashCount) = readVarInt(proof, pos)
    pos = p2
    println(s"hashCount=$hashCount")

    val hashes = new ArrayBuffer[String]
    for (_ <- 0 until hashCount){
      val (p, h) = readBytes(proof, pos, 32)
      hashes.addOne(DatatypeConverter.printHexBinary(h))
      pos = p
    }

    val (p3, flagsCount) = readVarInt(proof, pos)
    pos = p3
    val (p4, flagsReversed) = readBytes(proof, pos, flagsCount)
    pos = p4
    val flags = flagsReversed.reverse

    print(s"hashes=${hashes} flags=${flags} txcount=${txCount}")
    val rootNode = deserializeCoreFormatMerkleProof(hashes.toArray, flags, txCount)

    ElectrumMerkleProof(0, Array(), "", "")
  }


}
