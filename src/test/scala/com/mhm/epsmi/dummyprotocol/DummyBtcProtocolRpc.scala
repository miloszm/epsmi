package com.mhm.epsmi.dummyprotocol

import com.mhm.connectors.{BitcoinSConnector, BitcoindRpcExtendedClient}
import org.bitcoins.commons.jsonmodels.bitcoind.GetBlockHeaderResult
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.core.number.UInt32
import scodec.bits.HexStringSyntax

import scala.concurrent.Future

class DummyBtcProtocolRpc extends BitcoindRpcExtendedClient(BitcoinSConnector.bitcoindInstance, BitcoinSConnector.system){

  val DummyJsonrpcBlockchainHeight = 100000
  val blockchainHeight = DummyJsonrpcBlockchainHeight

  private def getDummyHashFromHeight(height: Int): DoubleSha256DigestBE = {
    val h = if (height == 0) "00"*32 else {
      s"$height" + "a"*(64 - height.toString.length)
    }
    DoubleSha256DigestBE.fromHex(h)
  }

  private def getHeightFromDummyHash(hash: String): Int = {
    if (hash == "00"*32) 0 else {
      val h = hash.substring(0, hash.indexOf('a'))
      h.toIntOption.getOrElse(0)
    }
  }

  override def getBestBlockHash: Future[DoubleSha256DigestBE] = {
    Future.successful(getDummyHashFromHeight(blockchainHeight))
  }

  override def getBlockHash(height: Int): Future[DoubleSha256DigestBE] = {
    if (height > blockchainHeight)
      throw new IllegalArgumentException(s"block height exceeds $blockchainHeight")
    else
      Future.successful(getDummyHashFromHeight(blockchainHeight))
  }

  override def getBlockHeader(headerHash: DoubleSha256DigestBE): Future[GetBlockHeaderResult] = {
    val height = getHeightFromDummyHash(headerHash.hex)
    val header = GetBlockHeaderResult(
      headerHash,
      blockchainHeight - height + 1,
      height = height,
      version = 536870912,
      versionHex = org.bitcoins.core.number.Int32.fromHex("20000000"),
      merkleroot = DoubleSha256DigestBE.fromHex("aa"*32),
      time = UInt32(height*100),
      mediantime = UInt32(height*100),
      nonce = UInt32(1),
      bits = UInt32.fromBytes(hex"207fffff"),
      difficulty = 4.656542373906925e-10,
      chainwork = "000000000000000000000000000000000000000000000"
        + "00000000000000000da",
      previousblockhash = None,
      nextblockhash = None
    )
    val previousBlockHash = if (height > 1) Some(getDummyHashFromHeight(height - 1))
      else if (height == 1) Some(getDummyHashFromHeight(0)) // #genesis block
      else if (height == 0) None
      else throw new IllegalArgumentException("height < 0")
    val header1 = header.copy(previousblockhash = previousBlockHash)
    val nextBlockHash = if (height < blockchainHeight) Some(getDummyHashFromHeight(height + 1)) else None
    val header2 = header1.copy(nextblockhash = nextBlockHash)
    Future.successful(header2)
  }
}
