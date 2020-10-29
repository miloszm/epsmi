package com.mhm.bitcoin.integration.test

import akka.actor.ActorSystem
import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.bitcoins.commons.jsonmodels.bitcoind.{LabelResult, RpcOpts}
import org.bitcoins.commons.serializers.JsonSerializers
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion, Client}
import org.bitcoins.rpc.client.v17.BitcoindV17RpcClient
import org.scalatest.FlatSpec
import play.api.libs.json.{JsArray, JsError, JsLookupResult, JsResult, JsString, JsSuccess, JsValue, Json, Reads, __}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt
import JsonSerializers._
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LabelPurpose
import org.bitcoins.commons.serializers.JsonReaders.LabelPurposeReads
import org.bitcoins.core.currency.Bitcoins
import play.api.libs.functional.syntax
import play.api.libs.functional.syntax.toFunctionalBuilderOps



class SetupTest extends FlatSpec {
  import play.api.libs.functional.syntax._
  import play.api.libs.json._

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
    val btcRpcClient = new BitcoindV17RpcClient(BitcoinSConnector.bitcoindInstance){
      override def version: BitcoindVersion = BitcoindVersion.Unknown
      override def getAddressesByLabel(label: String): Future[Map[BitcoinAddress, LabelResult]] = {
        bitcoindCall[Map[BitcoinAddress, LabelResult]]("getaddressesbylabel", List(JsString(label)))(mapAddressesByLabelReads2)
      }
      override def bitcoindCall[T](
        command: String,
        parameters: List[JsValue] = List.empty,
        printError: Boolean = true)(implicit reader: Reads[T]): Future[T] = {

        val request = buildRequest(instance, command, JsArray(parameters))
        val responseF = sendRequest(request)

        val payloadF: Future[JsValue] =
          responseF.flatMap(getPayload(_, command, request, parameters))

        payloadF.map { payload =>
          /**
           * These lines are handy if you want to inspect what's being sent to and
           * returned from bitcoind before it's parsed into a Scala type. However,
           * there will sensitive material in some of those calls (private keys,
           * XPUBs, balances, etc). It's therefore not a good idea to enable
           * this logging in production.
           */
          // logger.info(
          // s"Command: $command ${parameters.map(_.toString).mkString(" ")}")
          // logger.info(s"Payload: \n${Json.prettyPrint(payload)}")
          val x: JsLookupResult = (payload \ "result")
          //val xx = Json.parse("""{"1QLMf8HL5XmMkC72278Qcg42MDnu9gDZpQ": {"name":"", "purpose": "receive"}}""")
          val xxx = x.validate[T]
          parseResult(result = xxx,
            json = payload,
            printError = printError,
            command = command)
        }
      }
      private def parseResult[T](
        result: JsResult[T],
        json: JsValue,
        printError: Boolean,
        command: String
      ): T = {
        checkUnitError[T](result, json, printError)

        result match {
          case JsSuccess(value, _) => value
          case res: JsError =>
            (json \ "error").validate[BitcoindException] match {
              case JsSuccess(err, _) =>
                if (printError) {
                  logger.error(s"$err")
                }
                throw err
              case _: JsError =>
                val jsonResult = (json \ "result").get
                val errString =
                  s"Error when parsing result of '$command': ${JsError.toJson(res).toString}!"
                if (printError) logger.error(errString + s"JSON: $jsonResult")
                throw new IllegalArgumentException(
                  s"Could not parse JsResult: $jsonResult! Error: $errString")
            }
        }
      }

      // Catches errors thrown by calls with Unit as the expected return type (which isn't handled by UnitReads)
      private def checkUnitError[T](
        result: JsResult[T],
        json: JsValue,
        printError: Boolean): Unit = {
        if (result == JsSuccess(())) {
          (json \ "error").validate[BitcoindException] match {
            case JsSuccess(err, _) =>
              if (printError) {
                logger.error(s"$err")
              }
              throw err
            case _: JsError =>
          }
        }
      }
    }
    val resultFut = Setup.getScriptPubKeysToMonitor(btcRpcClient, config)
    val result = Await.result(resultFut, 20.seconds)
    println(s"setup result=$result")
  }

}
