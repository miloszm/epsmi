package com.mhm.wallet

import scodec.bits.ByteVector

object WalletUtil {

  def convertToStandardXpub(mpk: String): String = {
//    return btc.bip32_serialize((self.xpub_vbytes, *btc.bip32_deserialize(
//      mpk)[1:]))
    "xxxsjhs"
  }


  case class KeyRawTuple(
    vbytes: ByteVector,       // 0488b21e
    depth: Int,               // 0
    fingerprint: ByteVector,  // 00000000
    i: Int,                   // 0
    chaincode: ByteVector,    // d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec
    key: ByteVector           // 026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37
  )

  def bip32Serialize(rawTuple: KeyRawTuple): String = {
    ""

    // 'xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ'
  }

}
