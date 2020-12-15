package com.mhm.connectors

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{ImportMultiAddress, LabelPurpose}
import org.bitcoins.commons.jsonmodels.bitcoind.{DeriveAddressesResult, ImportMultiResult, LabelResult, TestMempoolAcceptResult}
import org.bitcoins.commons.serializers.JsonReaders._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.commons.serializers.JsonWriters._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto.ECPrivateKey
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.rpc.client.common.{BitcoindVersion, DescriptorRpc}
import org.bitcoins.rpc.client.v17.{BitcoindV17RpcClient, V17LabelRpc}
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future

case class ImportMultiRequestV18(
  desc: String,
  scriptPubKey: Option[ImportMultiAddress] = None,
  timestamp: UInt32,
  redeemscript: Option[ScriptPubKey] = None,
  witnessscript: Option[String] = None,
  pubkeys: Option[Vector[ScriptPubKey]] = None,
  keys: Option[Vector[ECPrivateKey]] = None,
  range: Array[Int],
  internal: Option[Boolean] = None,
  watchonly: Option[Boolean] = None,
  label: Option[String] = None,
  keypool: Option[Boolean] = None
)


class BitcoindRpcExtendedClient(bitcoindInstance: BitcoindInstance, actorSystem: ActorSystem)
  extends BitcoindV17RpcClient(bitcoindInstance)(implicitly[ActorSystem](actorSystem))
  with V17LabelRpc with DescriptorRpc {
  /**
   * have to override reads for LabelResult
   * as default in bitcoin-s was failing
   * on missing "name"
   */
  val labelResultWithDefault: Reads[LabelResult] =
    ((__ \ "name").readWithDefault("") and
    (__ \ "purpose").read[LabelPurpose])(LabelResult)

  def mapAddressesByLabelReadsWithDefault: Reads[Map[BitcoinAddress, LabelResult]] =
    Reads.mapReads[BitcoinAddress, LabelResult](s =>
    JsSuccess(BitcoinAddress.fromString(s)))(labelResultWithDefault)

  override def getAddressesByLabel(label: String): Future[Map[BitcoinAddress, LabelResult]] = {
    bitcoindCall[Map[BitcoinAddress, LabelResult]]("getaddressesbylabel", List(JsString(label)))(mapAddressesByLabelReadsWithDefault)
  }

  override def deriveAddresses(descriptor: String, range: Option[Vector[Double]]): Future[DeriveAddressesResult] = {
    val params =
      if (range.isDefined) List(JsString(descriptor), Json.toJson(range))
      else List(JsString(descriptor))
    val resultFut = bitcoindCall[Array[String]]("deriveaddresses", params)
    resultFut.map(result => DeriveAddressesResult(result.toVector.map(a => BitcoinAddress(a))))
  }


  implicit val importMultiRequestV18Writes: Writes[ImportMultiRequestV18] =
    Json.writes[ImportMultiRequestV18]


  def importMultiV18(requests: Vector[ImportMultiRequestV18], rescan: Boolean): Future[Vector[ImportMultiResult]] = {
    bitcoindCall[Vector[ImportMultiResult]](
      "importmulti",
      List(Json.toJson(requests), JsObject(Map("rescan" -> JsBoolean(rescan)))))
  }

  override def testMempoolAccept(
    transaction: Transaction,
    allowHighFees: Boolean = false): Future[TestMempoolAcceptResult] = {
    logger.info("calling testMempoolAccept")
    val feeParameter = JsNumber(0.1) // TODO we'd need to expose it, at the moment bitcoin-s supports deprecated interface
    val ret = bitcoindCall[Vector[TestMempoolAcceptResult]](
      "testmempoolaccept",
      List(JsArray(Vector(Json.toJson(transaction))), feeParameter))
      .map(_.head)
    logger.info(s"finished testMempoolAccept")
    ret
  }

  override def version: BitcoindVersion = BitcoindVersion.Unknown

  /**
   * from here to the EOF there is part duplicated from bitcoin-s Client.scala, for diagnostics reasons
   * can be safely deleted once troubleshooting is done
   */
  protected override def bitcoindCall[T](
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
       logger.info(s"Command: $command ${parameters.map(_.toString).mkString(" ")}")
       logger.info(s"Payload: \n${Json.prettyPrint(payload)}")
      parseResult(result = (payload \ resultKey).validate[T],
        json = payload,
        printError = printError,
        command = command)
    }
  }

  private val resultKey: String = "result"
  private val errorKey: String = "error"


  // Should both logging and throwing be happening?
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
        (json \ errorKey).validate[BitcoindException] match {
          case JsSuccess(err, _) =>
            if (printError) {
              logger.error(s"$err")
            }
            throw err
          case _: JsError =>
            val jsonResult = (json \ resultKey).get
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
      (json \ errorKey).validate[BitcoindException] match {
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
