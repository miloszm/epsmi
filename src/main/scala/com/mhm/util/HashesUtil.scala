package com.mhm.util

import com.mhm.util.MerkleProofOps.{doHash, hashDecode, hashDecodeRev}
import javax.xml.bind.DatatypeConverter

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

}
