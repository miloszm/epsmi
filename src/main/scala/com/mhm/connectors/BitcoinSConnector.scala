package com.mhm.connectors

import java.net.URI
import java.nio.ByteBuffer

import javax.xml.bind.DatatypeConverter
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import com.mhm.util.EpsmiDataUtil._

import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

trait BitcoinConnector {
  def getInfo(): String
  def getBestBlockHash(): String
}


object BitcoinSConnector extends BitcoinConnector {
  import org.bitcoins.core.config._
  import org.bitcoins.rpc.config._
  import org.bitcoins.rpc.client.common._

  val username = "foo" //this username comes from 'rpcuser' in your bitcoin.conf file
  val password = "bar" //this password comes from your 'rpcpassword' in your bitcoin.conf file

  val authCredentials = BitcoindAuthCredentials.PasswordBased(
    username = username,
    password = password
  )

  val bitcoindInstance = {
    BitcoindInstance (
      network = MainNet,
      uri = new URI(s"http://localhost:${MainNet.port}"),
      rpcUri = new URI(s"http://localhost:${MainNet.rpcPort}"),
      authCredentials = authCredentials
    )
  }

  implicit val ec: ExecutionContext = ExecutionContext.global

  val rpcCli = BitcoindRpcClient(bitcoindInstance)

  def getInfo(): String = {
    val infoFuture = rpcCli.getBlockChainInfo
    val info = Await.result(infoFuture, Duration(20, SECONDS))
    "" + info.blocks
  }

  def getBestBlockHash(): String = {
    val infoFuture = rpcCli.getBlockChainInfo
    val info = Await.result(infoFuture, Duration(20, SECONDS))
    info.bestblockhash.hex
  }

  def getLatestBlock: Future[Int] = rpcCli.getBlockChainInfo.map(_.blocks)

  def getBlock(blockHash: String): Future[Block] = {
    val h = DoubleSha256Digest(blockHash)
    rpcCli.getBlockRaw(h)
  }

  def getBlockHeaderHash(blockHeight: Int): Future[String] = {
    for {
      blockHash <- rpcCli.getBlockHash(blockHeight)
      blockHeader <- rpcCli.getBlockHeader(blockHash)
    } yield {
      val prevBlockHashArray = Array.fill[Byte](32)(0)

      blockHeader.previousblockhash match {
        case Some(b) => b.bytes.copyToArray(prevBlockHashArray, 0)
        case _ => ()
      }

      val merkleRootArray = Array.fill[Byte](32)(0)
      blockHeader.merkleroot.bytes.copyToArray(merkleRootArray, 0)

      println(s"prev block hash: ${DatatypeConverter.printHexBinary(prevBlockHashArray)}")
      println(s"merkle root hash: ${DatatypeConverter.printHexBinary(merkleRootArray)}")

      val head = ByteBuffer.allocate(80)
      // <i32s32sIII
      // little endian int | byte[32] | byte[32] | unsigned int | unsigned int | unsigned int
      head.put(intToArray(blockHeader.version))
      head.put(byteVectorOrZeroToArray(blockHeader.previousblockhash.map(_.bytes), 32))
      head.put(byteVectorToArray(blockHeader.merkleroot.bytes))
      head.put(uint32ToArray(blockHeader.time))
      head.put(uint32ToArray(blockHeader.bits))
      head.put(uint32ToArray(blockHeader.nonce))

      val headHex = DatatypeConverter.printHexBinary(head.array())
      headHex.toLowerCase
    }
  }
}
