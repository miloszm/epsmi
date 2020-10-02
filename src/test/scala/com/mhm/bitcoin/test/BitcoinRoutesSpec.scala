package com.mhm.bitcoin.test

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import akka.actor.typed.scaladsl.adapter._
import akka.http.scaladsl.model._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import com.mhm.bitcoin.{BitcoinApi, BitcoinRoutes}
import com.mhm.connectors.BitcoinConnector
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{Matchers, WordSpec}


object TestBitcoinConnector extends BitcoinConnector {
  override def getInfo(): String = "abc"
  override def getBestBlockHash(): String = "BESTBLOCKHASH"
}

class BitcoinRoutesSpec extends WordSpec with Matchers with ScalaFutures with ScalatestRouteTest {
  //#test-top

  // the Akka HTTP route testkit does not yet support a typed actor system (https://github.com/akka/akka-http/issues/2036)
  // so we have to adapt for now
  lazy val testKit = ActorTestKit()
  implicit def typedSystem = testKit.system
  override def createActorSystem(): akka.actor.ActorSystem =
    testKit.system.toClassic

  // Here we need to implement all the abstract members of BitcoinApi.
  // We use the real BitcoinApi Actor to test it while we hit the Routes,
  // but we could "mock" it by implementing it in-place or by using a TestProbe
  // created with testKit.createTestProbe()
  val bitcoinApi = testKit.spawn(BitcoinApi(TestBitcoinConnector))
  lazy val routes = new BitcoinRoutes(bitcoinApi).btcRoutes

  // use the json formats to marshal and unmarshall objects in the test
  //#set-up

  "BitcoinRoutes" should {
    "return basic info for (GET /info)" in {
      val request = HttpRequest(uri = "/info")
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[String] should ===("""{"text":"abc"}""")
      }
    }
    "return best block hash for (GET /bestblockhash)" in {
      val request = HttpRequest(uri = "/bestblockhash")
      request ~> routes ~> check {
        status should ===(StatusCodes.OK)
        contentType should ===(ContentTypes.`application/json`)
        entityAs[String] should ===("""{"hash":"BESTBLOCKHASH"}""")
      }
    }
  }
}