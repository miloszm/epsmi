package com.mhm.connectors

import java.net.URI

import akka.actor.ActorSystem
import org.bitcoins.crypto.DoubleSha256Digest
import org.bitcoins.core.protocol.blockchain.Block
import org.bitcoins.rpc.client.common.BitcoindRpcClient.ActorSystemName

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

  /**
   * we have to use internal bitcoin-s actor system name here
   * as due to weridness of bitcoin-s it does not cover
   * all APIs
   * since we have 2 clients, one "normal" and one "extended"
   * we have to have the actor system here, hence we need to
   * use the name here
   */
  val actorSystemName = "bitcoind-rpc-client-created-by-bitcoin-s"

  implicit val system = ActorSystem.create(actorSystemName)

  //val rpcCli = BitcoindRpcClient.withActorSystem(bitcoindInstance)

  /**
   * extended rpc client with more APIs - due to the strangeness of bitcoin-s not covering
   * newer APIs easily, for example, getaddressesbylabel
   */
  val rpcCli = new BitcoindRpcExtendedClient(bitcoindInstance, implicitly[ActorSystem])

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
}
