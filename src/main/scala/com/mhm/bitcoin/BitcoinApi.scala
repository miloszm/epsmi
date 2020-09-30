package com.mhm.bitcoin

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import com.mhm.connectors.BitcoinSConnector


final case class BlockchainInfo(text: String)
final case class BestBlockHash(hash: String)

object BitcoinApi {
  // actor protocol
  sealed trait BitcoinCommand
  final case class GetBlockchainInfo(replyTo: ActorRef[BlockchainInfo]) extends BitcoinCommand
  final case class GetBestBlockHash(replyTo: ActorRef[BestBlockHash]) extends BitcoinCommand

  def apply(): Behavior[BitcoinCommand] = btcQuestioner()

  private def btcQuestioner(): Behavior[BitcoinCommand] =
    Behaviors.receiveMessage {
      case GetBlockchainInfo(replyTo) =>
        val info = BitcoinSConnector.getInfo()
        replyTo ! BlockchainInfo(info)
        Behaviors.same
      case GetBestBlockHash(replyTo) =>
        val hash = BitcoinSConnector.getBestBlockHash()
        replyTo ! BestBlockHash(hash)
        Behaviors.same
    }
}
