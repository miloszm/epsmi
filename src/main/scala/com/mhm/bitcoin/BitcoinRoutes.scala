package com.mhm.bitcoin

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Route

import scala.concurrent.Future
import com.mhm.bitcoin.BitcoinApi._
import akka.actor.typed.ActorRef
import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.AskPattern._
import akka.util.Timeout



class BitcoinRoutes(bitcoinApi: ActorRef[BitcoinApi.BitcoinCommand])(implicit val system: ActorSystem[_]) {
    import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
    import com.mhm.bitcoin.JsonFormats._

    // If ask takes more time than this to complete the request is failed
    private implicit val timeout = Timeout.create(system.settings.config.getDuration("my-app.routes.ask-timeout"))

    def getInfo(): Future[BlockchainInfo] =
      bitcoinApi.ask(GetBlockchainInfo)

    def getBestBlockHash(): Future[BestBlockHash] =
      bitcoinApi.ask(GetBestBlockHash)

    //#all-routes
    //#users-get-post
    //#users-get-delete
    val btcRoutes: Route =
    pathPrefix("info") {
      concat(
        pathEnd {
          concat(
            get {
              complete(getInfo())
            })
        }
      )
    } ~
    pathPrefix("bestblockhash") {
      concat(
        pathEnd {
          concat(
            get {
              complete(getBestBlockHash())
            })
        }
      )
    }
}
