package com.mhm.wallet

import com.mhm.util.BaseOps.{decodeBytesBase256, encodeBase256}
import com.mhm.util.{BaseOps, HashOps}
import scodec.bits.{ByteVector, HexStringSyntax}

object WalletUtil {

  def convertToStandardXpub(mpk: String, xpubVBytes: ByteVector): String = {
    val xkrt = bip32Deserialize(mpk).copy(vbytes = xpubVBytes)
    bip32Serialize(xkrt)
  }


  case class XKeyRawTuple(
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


  def bip32Serialize(rawTuple: XKeyRawTuple): String = {
    val iEnc = encodeBase256(BigInt(rawTuple.i), 4)
    val chaincodeEnc = encodeBase256(hashToInt(rawTuple.chaincode), 32)
    val keydata = if (PRIVATE.contains(rawTuple.vbytes)) 0.toByte +: rawTuple.key.init else rawTuple.key
    val bindata = rawTuple.vbytes ++ ByteVector((rawTuple.depth % 256).toByte) ++ rawTuple.fingerprint ++ iEnc ++ chaincodeEnc ++ keydata
    BaseOps.changebase256to58(bindata ++ HashOps.binDblSha256(bindata).take(4))
  }

  def bip32Deserialize(data: String): XKeyRawTuple = {
    val dataIn = BaseOps.changebase58to256(data)
    if (HashOps.binDblSha256(dataIn.dropRight(4)).take(4) != dataIn.takeRight(4))
      throw new IllegalStateException("Invalid checksum")
    val vbytes = dataIn.take(4)
    val depth: Int = dataIn(4).toInt & 0xff
    val fingerprint = dataIn.slice(5, 9)
    val i = decodeBytesBase256(dataIn.slice(9, 13)).toInt
    val chaincode = dataIn.slice(13, 45)
    val key = if (PRIVATE.contains(vbytes)) dataIn.slice(46, 78) :+ 0x01.toByte else dataIn.slice(45, 78)
    XKeyRawTuple(vbytes, depth, fingerprint, i, chaincode, key)
  }

  def hashToInt(v: ByteVector): BigInt = {
    if (v.length == 40 || v.length == 64)
      BaseOps.decodeBytesBase16(v)
    else
      BaseOps.decodeBytesBase256(v)
  }



}
