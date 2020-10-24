package com.mhm.util

import java.nio.ByteBuffer
import java.security.MessageDigest

import com.mhm.api4electrum.Api4ElectrumCore.ElectrumMerkleProof
import com.mhm.util.EpsmiDataUtil.intCeilLog2
import javax.xml.bind.DatatypeConverter

import scala.collection.mutable.{ArrayBuffer, ListBuffer}


//case class MerkleNode(valueOpt: Option[String], left: Option[MerkleNode]=None, right: Option[MerkleNode]=None){
//  def isTuple: Boolean = valueOpt.isEmpty
//  def value: String = valueOpt.getOrElse("") // TODO else should throw
//}

object MerkleProofOps {
  def isBitSet(b: Byte, pos: Int): Boolean = (b & (1 << pos)) != 0

  def hashDecode(hs: String): Array[Byte] = {
    DatatypeConverter.parseHexBinary(hs)
  }

  def hashEncode(b: Array[Byte]): String = {
    DatatypeConverter.printHexBinary(b)
  }

  def sha256(a: Array[Byte]): Array[Byte] = {
    MessageDigest.getInstance("SHA-256")
      .digest(a)
  }

  def doHash(a: Array[Byte]): Array[Byte] = {
    sha256(sha256(a))
  }

  def calcTreeWidth(height: Int, txCount: Int): Int = {
    (txCount + (1 << height) - 1) >> height
  }

  def getNodeHash(nodeValue: String): String = {
    if (nodeValue.startsWith("tx"))
      nodeValue.split(":")(2)
    else
      nodeValue
  }

  def expandTreeHashing(node: TupleMerkleNode): MerkleNode = {
    val left = node.left
    val right = node.right
    val hashLeft = if (left.isTuple)
      expandTreeHashing(left.asInstanceOf[TupleMerkleNode])
    else
      StrMerkleNode(getNodeHash(left.value))
    val hashRight = if (right.isTuple)
      expandTreeHashing(right.asInstanceOf[TupleMerkleNode])
    else
      StrMerkleNode(getNodeHash(right.value))
    val hs = hashEncode(doHash(hashDecode(hashLeft.value) ++ hashDecode(hashRight.value)))
    StrMerkleNode(hs)
  }


  case class HashListIter(hashList: List[String]){
    var pos = 0
    def next(): String = {
      val r = hashList(pos)
      pos += 1
      r
    }
  }

  case class FlagsIter(flagsList: List[Boolean]){
    var pos = 0
    def next(): Boolean = {
      val r = flagsList(pos)
      pos += 1
      r
    }
  }


  def descendMerkleTree(hashList: HashListIter, flags: FlagsIter, height: Int, txCount: Int, pos: Int): MerkleNode = {
    val flag = flags.next()
    if (height > 0){
      if (flag){
        val left = descendMerkleTree(hashList, flags, height-1, txCount, pos*2)
        val right: MerkleNode = if (pos*2+1 < calcTreeWidth(height-1, txCount)) {
          descendMerkleTree(hashList, flags, height - 1, txCount, pos * 2 + 1)
        } else {
          if (left.isTuple)
            expandTreeHashing(left.asInstanceOf[TupleMerkleNode])
          else
            left
        }
        TupleMerkleNode(left, right)
      }
      else {
        StrMerkleNode(hashList.next())
      }
    }
    else {
      val hs = hashList.next()
      StrMerkleNode(if (flag) s"tx:$pos:$hs" else hs)
    }
  }

  def deserializeCoreFormatMerkleProof(hashList: Array[String], flagValue: Array[Byte], txCount: Int): MerkleNode = {
    val treeDepth: Int = intCeilLog2(txCount)

    val flags = for {b <- flagValue; i <- 0 to 7} yield isBitSet(b, i)

    descendMerkleTree(HashListIter(hashList.toList), FlagsIter(flags.toList), treeDepth, txCount, 0)
  }


  /**
   * Recurse down into the tree, adding hashes to the result list in depth order
   */
  def expandTreeElectrumFormatMerkleProof(node: TupleMerkleNode, accumulator: List[String]): List[String] = {
    val left = node.left
    val right = node.right
    val acc1 = accumulator ++ (if (left.isTuple) expandTreeElectrumFormatMerkleProof(left.asInstanceOf[TupleMerkleNode], accumulator) else Nil)
    val acc2 = acc1 ++ (if (right.isTuple) expandTreeElectrumFormatMerkleProof(right.asInstanceOf[TupleMerkleNode], accumulator) else Nil)
    val acc3 = if (!left.isTuple) acc2 :+ left.value else acc2
    if (!right.isTuple) acc3 :+ right.value else acc3
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
    val merkleRoot = Array.ofDim[Byte](32)
    Array.copy(proof, 36, merkleRoot, 0, 32)
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

    println(s"hashes=$hashes flags=$flags txcount=$txCount")
    val rootNode = deserializeCoreFormatMerkleProof(hashes.toArray, flags, txCount)
    println(s"scala root node after deserialisation: $rootNode")

    if (rootNode.isTuple) {
      val hashesList = ListBuffer(expandTreeElectrumFormatMerkleProof(rootNode.asInstanceOf[TupleMerkleNode], Nil): _*)
      val tx = hashesList.remove(if (hashesList(1).startsWith("tx")) 1 else 0)
      val tokens = tx.split(":")
      if (hashesList.head.startsWith("tx")){
        val h0 = hashesList.remove(0)
        hashesList.prepend(tokens(2))
      }
      val txPos = tokens(1).toIntOption.getOrElse(0)
      val txId = tokens(2)
      ElectrumMerkleProof(txPos, hashesList.toArray, txId, DatatypeConverter.printHexBinary(merkleRoot.reverse))
    }
    else {
      val txId = rootNode.value.substring(5) //remove the "tx:0:"
      ElectrumMerkleProof(0, Array(), txId, txId)
    }
  }


}
