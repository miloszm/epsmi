package com.mhm.main

import org.bitcoins.core.crypto.{TransactionSignatureCreator, TxSigComponent}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.transaction.{BaseTransaction, Transaction, TransactionInput, TransactionOutPoint, TransactionOutput}
import org.bitcoins.crypto.{DoubleSha256DigestBE, ECPrivateKey}
import scodec.bits.{BitVector, ByteVector}

import scala.math.BigInt
import org.bitcoins.core.number.{Int32, Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{CLTVScriptPubKey, EmptyScriptSignature, P2PKHScriptPubKey, P2PKHScriptSignature, P2SHScriptPubKey, ScriptPubKey}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.{HashType, SIGHASH_ALL}

trait BitcoinSUtil {

  def decodeHex(hex: String): ByteVector = {
    if (hex.isEmpty) ByteVector.empty else ByteVector.fromHex(hex).get
  }

  def encodeHex(bytes: ByteVector): String = bytes.toHex

  def encodeHex(byte: Byte): String = encodeHex(ByteVector(byte))

  /**
   * Encodes a long number to a hex string, pads it with an extra '0' char
   * if the hex string is an odd amount of characters.
   */
  def encodeHex(long: Long): String = {
    val hex = long.toHexString.length % 2 match {
      case 1      => "0" + long.toHexString
      case _: Int => long.toHexString
    }
    addPadding(16, hex)
  }

  def encodeHex(int: Int): String = {
    val hex = int.toHexString.length % 2 match {
      case 1      => "0" + int.toHexString
      case _: Int => int.toHexString
    }
    addPadding(8, hex)
  }

  def encodeHex(short: Short): String = {
    val bytes = ByteVector.fromShort(short)
    encodeHex(bytes)
  }

  def encodeHex(bigInt: BigInt): String =
    BitcoinSUtil.encodeHex(ByteVector(bigInt.toByteArray))

  /** Tests if a given string is a hexadecimal string. */
  def isHex(str: String): Boolean = {
    //check valid characters & hex strings have to have an even number of chars
    str.matches("^[0-9a-f]+$") && (str.length % 2 == 0)
  }

  /** Converts a two character hex string to its byte representation. */
  def hexToByte(hex: String): Byte = {
    require(hex.length == 2)
    BitcoinSUtil.decodeHex(hex).head
  }

  /** Flips the endianness of the give hex string. */
  def flipEndianness(hex: String): String = flipEndianness(decodeHex(hex))

  /** Flips the endianness of the given sequence of bytes. */
  def flipEndianness(bytes: ByteVector): String = encodeHex(bytes.reverse)

  /**
   * Adds the amount padding bytes needed to fix the size of the hex string
   * for instance, ints are required to be 4 bytes. If the number is just 1
   * it will only take 1 byte. We need to pad the byte with an extra 3 bytes so the result is
   * 00000001 instead of just 1.
   */
  private def addPadding(charactersNeeded: Int, hex: String): String = {
    val paddingNeeded = charactersNeeded - hex.length
    val padding = Vector.fill(paddingNeeded)("0")
    val paddedHex = padding.mkString + hex
    paddedHex
  }

  /** Converts a byte to a bit vector representing that byte */
  def byteToBitVector(byte: Byte): BitVector = {
    BitVector.fromByte(byte)
  }

  /** Checks if the bit at the given index is set */
  def isBitSet(byte: Byte, index: Int): Boolean = ((byte >> index) & 1) == 1

  /** Converts a sequence of bit vectors to a sequence of bytes */
  def bitVectorToBytes(bits: BitVector): ByteVector = {
    bits.bytes
  }

//  def toByteVector[T <: NetworkElement](h: Seq[T]): ByteVector = {
//    h.foldLeft(ByteVector.empty)(_ ++ _.bytes)
//  }

}

object BitcoinSUtil extends BitcoinSUtil



//{
//"txid": "dd91b944e8b743b1cdb8d106161e8d1f1a666597cc16a6291f7fd4baca891508",
//"vout": 1,
//"address": "mtj9vxnzy9rP9A2fUJ3bZBpdYFWn6w1QQC",
//"label": "electrum-watchonly-addresses",
//"scriptPubKey": "76a91490e8571c1e4d5f37e736474ca076b67b11ff778788ac",
//"amount": 0.00089220,
//"confirmations": 40279,
//"spendable": false,
//"solvable": true,
//"desc": "pkh([520889b3/1/1]0364eabda399f7bfee4bcc685e7f9a3ccdd4f6b540962901ee831690d497f64292)#4mve5vpc",
//"safe": true
//}


// mtj9vxnzy9rP9A2fUJ3bZBpdYFWn6w1QQC	p2pkh:cN9XS1bFNhMmmvXTNudnwZd7zyuRwCk4HmEVy4xbSxtPArC4KcoE

//cN9XS1bFNhMmmvXTNudnwZd7zyuRwCk4HmEVy4xbSxtPArC4KcoE
//cQ783r3vMAM6AgKEqmSUNLxPKwmg5o6ErUTGPp8jLUhRaUAA11N1

object Playground extends App {

  def printTx(tx: Transaction): Unit ={
    println(s"tx[txId]=${tx.txId}")
    println(s"tx[inputs]=${tx.inputs}")
    println(s"tx[outputs]=${tx.outputs}")
  }

  val FundingTxid = "dd91b944e8b743b1cdb8d106161e8d1f1a666597cc16a6291f7fd4baca891508"
  val FundingVoutIndex = 0
  val FundingAddress = "mtj9vxnzy9rP9A2fUJ3bZBpdYFWn6w1QQC"
  val FundingScriptPubKey = "76a91490e8571c1e4d5f37e736474ca076b67b11ff778788ac"
  val FundingPrivKey = "cN9XS1bFNhMmmvXTNudnwZd7zyuRwCk4HmEVy4xbSxtPArC4KcoE"
  val LockUntil = 1614459600L
  val OutputAmount = 69220


  val util = BitcoinSUtil
  val outpoint = TransactionOutPoint(DoubleSha256DigestBE.fromHex(FundingTxid), UInt32(FundingVoutIndex))
  val unsignedInput = TransactionInput(outpoint, EmptyScriptSignature, UInt32(0))
  val fundingPrivKey = org.bitcoins.core.crypto.ECPrivateKeyUtil.fromWIFToPrivateKey(FundingPrivKey)
  val fundingPublicKey = fundingPrivKey.publicKey
  val fundingScriptPubKey = ScriptPubKey(FundingScriptPubKey)

  // construct CLTV transaction
  val cltvPrivKey = ECPrivateKey()
  println(s"cltvPrivKey=${cltvPrivKey.hex}")
  val cltvPubKey = cltvPrivKey.publicKey
  println(s"cltvPubKey=${cltvPubKey.hex}")
  val p2pkh = P2PKHScriptPubKey(cltvPubKey)
  println(s"p2pkh=${p2pkh.asm.mkString(" ")}")

  val redeemScript = CLTVScriptPubKey(ScriptNumber(LockUntil), p2pkh)
  println(s"redeemScript=${redeemScript.asm.mkString(" ")}")
  val p2sh = P2SHScriptPubKey(redeemScript)
  val cltvOutput = TransactionOutput(Satoshis(Int64(OutputAmount)), p2sh)
  val unsignedTx = Transaction.newBuilder.setVersion(Int32(1)).addInputs(Seq(unsignedInput)).addOutputs(Seq(cltvOutput)).setLockTime(UInt32(0)).result().toBaseTransaction

  println("unsigned tx:")
  printTx(unsignedTx)

  val txOutput = TransactionOutput(Satoshis(Int64(OutputAmount)), fundingScriptPubKey)
  val txSigComponent = TxSigComponent(unsignedTx, UInt32(0), txOutput, Policy.standardScriptVerifyFlags)
  val sig = TransactionSignatureCreator.createSig(txSigComponent, fundingPrivKey, HashType.sigHashAll)
  val scriptSig = P2PKHScriptSignature(sig, fundingPublicKey)
  val signedInput = TransactionInput(outpoint, scriptSig, UInt32(0))
  val signedTx = Transaction.newBuilder.setVersion(Int32(1)).addInputs(Seq(signedInput)).addOutputs(Seq(cltvOutput)).setLockTime(UInt32(0)).result().toBaseTransaction

  println("signed tx:")
  printTx(signedTx)

  println("tx to be pushed:")
  println(signedTx.hex)
}
