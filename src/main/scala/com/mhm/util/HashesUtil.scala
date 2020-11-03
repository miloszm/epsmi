package com.mhm.util

import java.security.MessageDigest

import com.mhm.util.MerkleProofOps.hashDecodeRev
import scodec.bits.ByteVector

object HashesUtil {

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

  def doHash(a: Array[Byte]): Array[Byte] = {
    sha256(sha256(a))
  }

  def binDblSha256(v: ByteVector): ByteVector = {
    ByteVector(doHash(v.toArray))
  }

}
