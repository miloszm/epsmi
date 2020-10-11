package com.mhm.connectors

import java.net.URI

import javax.xml.bind.DatatypeConverter
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

  def getBlockHeaderHash(blockHeight: Int): String = {
    val blockHash = Await.result(rpcCli.getBlockHash(blockHeight), Duration(20, SECONDS))
    val blockHeader = Await.result(rpcCli.getBlockHeader(blockHash), Duration(20, SECONDS))
    val prevBlockHash = blockHeader.nextblockhash.getOrElse("00"*32)

    //val x = DatatypeConverter.parseHexBinary("AA")

  }
}
