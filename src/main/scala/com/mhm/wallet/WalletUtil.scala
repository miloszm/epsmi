package com.mhm.wallet

import com.mhm.util.{BaseOps, HashesUtil}
import scodec.bits.{ByteVector, HexStringSyntax}

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


  val MAINNET_PRIVATE = hex"0488ADE4"
  val TESTNET_PRIVATE = hex"04358394"
  val PRIVATE = Set(MAINNET_PRIVATE, TESTNET_PRIVATE)


  def bip32Serialize(rawTuple: KeyRawTuple): String = {
    val iEnc = BaseOps.encodeBase256(BigInt(rawTuple.i), 4)
    val chaincodeEnc = BaseOps.encodeBase256(hashToInt(rawTuple.chaincode), 32)
    val keydata = if (PRIVATE.contains(rawTuple.vbytes)) 0.toByte +: rawTuple.key.init else rawTuple.key
    val bindata = rawTuple.vbytes ++ ByteVector((rawTuple.depth % 256).toByte) ++ rawTuple.fingerprint ++ iEnc ++ chaincodeEnc ++ keydata
    BaseOps.changebase256to58(bindata ++ HashesUtil.binDblSha256(bindata).take(4))
  }

  def hashToInt(v: ByteVector): BigInt = {
    if (v.length == 40 || v.length == 64)
      BaseOps.decodeBytesBase16(v)
    else
      BaseOps.decodeBytesBase256(v)
  }



}
