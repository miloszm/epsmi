package com.mhm.bitcoin.integration.test

import akka.actor.ActorSystem
import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LabelPurpose
import org.bitcoins.commons.serializers.JsonReaders.LabelPurposeReads
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.V17LabelRpc
import org.scalatest.FlatSpec

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}



class SetupTest extends FlatSpec {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

  /**
   * have to override reads for LabelResult
   * as default in bitcoin-s was failing
   * on missing "name"
   */
  val labelResult2: Reads[LabelResult] =
    ((__ \ "name").readWithDefault("") and
     (__ \ "purpose").read[LabelPurpose])(LabelResult)

  def mapAddressesByLabelReads2: Reads[
    Map[BitcoinAddress, LabelResult]] =
    Reads.mapReads[BitcoinAddress, LabelResult](s =>
      JsSuccess(BitcoinAddress.fromString(s)))(labelResult2)


  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    implicit val system = ActorSystem.create("bitcoind-rpc-client-created-by-bitcoin-s")
    val btcRpcClient = new BitcoindRpcClient(BitcoinSConnector.bitcoindInstance) with V17LabelRpc {
      /**
       * need to override version as bitcoin-s fails on current btc not being v17
       */
      //override def version: BitcoindVersion = BitcoindVersion.Unknown

      /**
       * need to override get addresses by label to be able to override deserialization
       * by the way, maybe I can remove this v17 stuff now
       */
      override def getAddressesByLabel(label: String): Future[Map[BitcoinAddress, LabelResult]] = {
        bitcoindCall[Map[BitcoinAddress, LabelResult]]("getaddressesbylabel", List(JsString(label)))(mapAddressesByLabelReads2)
      }
    }

    val resultFut = Setup.getScriptPubKeysToMonitor(btcRpcClient, config)
    val result = Await.result(resultFut, 20.seconds)
    println(s"setup result=$result")
  }

}
