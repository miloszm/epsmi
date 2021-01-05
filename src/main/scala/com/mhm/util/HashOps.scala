package com.mhm.util

import java.security.MessageDigest

import com.mhm.common.model.HashHeight
import com.mhm.util.MerkleProofOps.hashDecodeRev
import javax.xml.bind.DatatypeConverter
import javax.xml.bind.DatatypeConverter.printHexBinary
import scodec.bits.ByteVector

object HashOps {

  // TODO convert to ByteVector
  def hashMerkleRoot(merkle: Array[String], targetHash: String, pos: Int): String = {
    var h = hashDecodeRev(targetHash)
    for (i <- merkle.indices){
      val item = merkle(i)
      h = if (((pos >> i) & 1) != 0)
        doHash(hashDecodeRev(item) ++ h)
      else
        doHash(h ++ hashDecodeRev(item))
    }
    MerkleProofOps.hashEncodeRev(h)
  }

  def sha256(a: Array[Byte]): Array[Byte] = {
    MessageDigest.getInstance("SHA-256")
      .digest(a)
  }

  def sha256BV(a: ByteVector): ByteVector = {
    ByteVector(sha256(a.toArray))
  }

  def doHash(a: Array[Byte]): Array[Byte] = {
    sha256(sha256(a))
  }

  def binDblSha256(v: ByteVector): ByteVector = {
    ByteVector(doHash(v.toArray))
  }

  def script2ScriptHash(s: String): String = {
    val h = sha256BV(ByteVector.fromHex(s).getOrElse(throw new IllegalArgumentException(s"could not convert to hex: '$s'"))).take(32)
    h.reverse.toHex
  }

//  # 'result' field in the blockchain.scripthash.subscribe method
//  # reply uses this as a summary of the address
  def getStatusElectrum(h: List[HashHeight]): String = {
    if (h.isEmpty) "" else {
      val status = h.map(hh => s"${hh.hash}:${hh.height}:").mkString
      printHexBinary(sha256(status.getBytes)).toLowerCase
    }
  }

}
