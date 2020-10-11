package com.mhm.connectors

import java.net.URI
import java.nio.ByteBuffer

import javax.xml.bind.DatatypeConverter
import org.bitcoins.core.number.UInt32
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block

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

  def reverse(array: Array[Byte]): Array[Byte] = {
    val l = array.length
    val a = Array.ofDim[Byte](l)
    for (i <- a.indices) a(i) = array(l - 1 - i)
    a
  }

  def intToArray(i: Int): Array[Byte] = {
    val b = ByteBuffer.allocate(4)
    b.putInt(i)
    reverse(b.array())
  }

  def uint32ToArray(i: UInt32): Array[Byte] = {
    val a = Array.ofDim[Byte](4)
    val b = ByteBuffer.allocate(4)
    i.bytes.copyToArray(a, 0)
    b.put(a)
    reverse(a)
  }

  def getBlockHeaderHash(blockHeight: Int): String = {
    val blockHash = Await.result(rpcCli.getBlockHash(blockHeight), Duration(20, SECONDS))
    val blockHeader = Await.result(rpcCli.getBlockHeader(blockHash), Duration(20, SECONDS))
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
    //head.put("<i32s32sIII".getBytes)
    head.put(intToArray(blockHeader.version))
    head.put(reverse(prevBlockHashArray))
    head.put(reverse(merkleRootArray))
    head.put(uint32ToArray(blockHeader.time))
    head.put(uint32ToArray(blockHeader.bits))
    head.put(uint32ToArray(blockHeader.nonce))

    val headHex = DatatypeConverter.printHexBinary(head.array())
    val s = s"""{"hex":\"$headHex\", "height":${blockHeader.height}""""
    s
    headHex
  }
}
