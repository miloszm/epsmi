package com.mhm.connectors

import java.net.URI

import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block

import scala.concurrent.duration.{Duration, SECONDS}
import scala.concurrent.{Await, ExecutionContext, Future}

object BitcoinSConnector {
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

  def getLatestBlock: Future[Int] = rpcCli.getBlockChainInfo.map(_.blocks)

  def getBlock(blockHash: String): Future[Block] = {
    val h = DoubleSha256Digest(blockHash)
    rpcCli.getBlockRaw(h)
  }
}
