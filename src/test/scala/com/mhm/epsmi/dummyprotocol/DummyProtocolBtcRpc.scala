package com.mhm.epsmi.dummyprotocol

import com.mhm.connectors.{BitcoinSConnector, BitcoindRpcExtendedClient}
import com.mhm.epsmi.dummyprotocol.DummyProtocolBtcRpc.DummyJsonrpcBlockchainHeight
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.bitcoins.commons.jsonmodels.bitcoind.{GetBlockHeaderResult, GetTransactionResult}
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.core.number.UInt32
import scodec.bits.HexStringSyntax

import scala.concurrent.Future

object DummyProtocolBtcRpc {
  val DummyJsonrpcBlockchainHeight = 100000
}

case class DummyProtocolBtcRpc() extends BitcoindRpcExtendedClient(TestBitcoinSConnector.bitcoindInstance, TestBitcoinSConnector.system){

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
    if (height > DummyJsonrpcBlockchainHeight)
      throw new IllegalArgumentException(s"height > $DummyJsonrpcBlockchainHeight")
    Future.successful(getDummyHashFromHeight(height))
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
    val previousBlockHash = height match {
      case h if h > 1 => Some(getDummyHashFromHeight(height - 1))
      case h if h == 1 => Some(getDummyHashFromHeight(0)) // #genesis block
      case h if h == 0 => None
      case _ => throw new IllegalArgumentException("height < 0")
    }
    val header1 = header.copy(previousblockhash = previousBlockHash)
    val nextBlockHash = if (height < blockchainHeight) Some(getDummyHashFromHeight(height + 1)) else None
    val header2 = header1.copy(nextblockhash = nextBlockHash)
    Future.successful(header2)
  }

  override def getTransaction(txid: DoubleSha256DigestBE, watchOnly: Boolean): Future[GetTransactionResult] = ???
}
