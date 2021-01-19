package com.mhm.util

import java.nio.ByteBuffer

import com.mhm.common.model.ElectrumMerkleProof
import com.mhm.util.BaseOps.byteToUnsignedInt
import com.mhm.util.EpsmiDataOps.intCeilLog2
import com.mhm.util.HashOps.doHash
import javax.xml.bind.DatatypeConverter

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object MerkleProofOps {
  def isBitSet(b: Byte, pos: Int): Boolean = (b & (1 << pos)) != 0

  def hashDecode(hs: String): Array[Byte] = {
    DatatypeConverter.parseHexBinary(hs)
  }

  def hashEncode(b: Array[Byte]): String = {
    DatatypeConverter.printHexBinary(b)
  }

  def hashDecodeRev(hs: String): Array[Byte] = {
    DatatypeConverter.parseHexBinary(hs).reverse
  }

  def hashEncodeRev(b: Array[Byte]): String = {
    DatatypeConverter.printHexBinary(b.reverse)
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
    val hs = hashEncodeRev(doHash(hashDecodeRev(hashLeft.value) ++ hashDecodeRev(hashRight.value)))
    StrMerkleNode(hs)
  }

  case class ListIter[T](list: List[T]){
    var pos = 0
    def next(): T = {
      val r = list(pos)
      pos += 1
      r
    }
  }

  def descendMerkleTree(hashList: ListIter[String], flags: ListIter[Boolean], height: Int, txCount: Int, pos: Int): MerkleNode = {
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
    val treeDepth = intCeilLog2(txCount)
    val flags = for {b <- flagValue; i <- 0 to 7} yield isBitSet(b, i)
    descendMerkleTree(ListIter(hashList.toList), ListIter(flags.toList), treeDepth, txCount, 0)
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

  case class ReadResult[T](newPos: Int, value: T)

  def readVarInt(buf: Array[Byte], pos: Int): ReadResult[Long] = {
    val v = byteToUnsignedInt(buf(pos))
    if ( v < 253)
      ReadResult(pos + 1, v)
    else {
      val numBytes = Math.pow(2.0, v - 252).toInt
      val newPos = pos + 1 + numBytes
      val intBytes = ByteBuffer.wrap(buf.slice(newPos - numBytes, newPos).reverse)
      numBytes match {
        case 2 => ReadResult(newPos, intBytes.getShort)
        case 4 => ReadResult(newPos, intBytes.getInt)
        case 8 => ReadResult(newPos, intBytes.getLong)
      }
    }
  }

  def readAsInt(buf: Array[Byte], pos: Int, bytez: Int): ReadResult[Int] = {
    val newPos = pos + bytez
    val intBytes = ByteBuffer.wrap(buf.slice(pos, pos + bytez).reverse)
    ReadResult(newPos, intBytes.getInt)
  }

  def readBytes(buf: Array[Byte], pos: Int, bytez: Int): ReadResult[Array[Byte]] = {
    val newPos = pos + bytez
    ReadResult(newPos, buf.slice(newPos - bytez, newPos).reverse)
  }

  case class CurrentPositionHashesPair(curPos: Int, hashes: List[String])

  def readHashes(curPos: Int, hashCount: Int, proof: Array[Byte]): CurrentPositionHashesPair = {
    @tailrec
    def doReadHashes(curIndex: Int, curPos: Int, hashes: List[String]): CurrentPositionHashesPair = curIndex match {
      case i if i == hashCount => CurrentPositionHashesPair(curPos, hashes)
      case i =>
        val ReadResult(p, h) = readBytes(proof, curPos, 32)
        doReadHashes(i + 1, p, hashes :+ DatatypeConverter.printHexBinary(h))
    }
    doReadHashes(0, curPos, Nil)
  }

  def convertCoreToElectrumMerkleProof(merkleBlockHex: String): ElectrumMerkleProof = {
    val pos = 80
    val proof = DatatypeConverter.parseHexBinary(merkleBlockHex)
    val merkleRoot = Array.ofDim[Byte](32)
    Array.copy(proof, 36, merkleRoot, 0, 32)
    val ReadResult(pos1, txCount) = readAsInt(proof, pos, 4)
    val ReadResult(pos2, hashCount) = readVarInt(proof, pos1)
    val CurrentPositionHashesPair(pos3, hashes) = readHashes(pos2, hashCount.toInt, proof)
    val ReadResult(pos4, flagsCount) = readVarInt(proof, pos3)
    val ReadResult(_, flagsReversed) = readBytes(proof, pos4, flagsCount.toInt)
    val rootNode = deserializeCoreFormatMerkleProof(hashes.toArray, flagsReversed.reverse, txCount)

    if (rootNode.isTuple) {
      val hashesList = ListBuffer(expandTreeElectrumFormatMerkleProof(rootNode.asInstanceOf[TupleMerkleNode], Nil): _*)
      val tx = hashesList.remove(if (hashesList(1).startsWith("tx")) 1 else 0)
      val tokens = tx.split(":")
      if (hashesList.head.startsWith("tx")){
        hashesList.remove(0)
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
