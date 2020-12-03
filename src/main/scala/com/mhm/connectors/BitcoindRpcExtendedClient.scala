package com.mhm.connectors

import akka.actor.ActorSystem
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.LabelPurpose
import org.bitcoins.commons.jsonmodels.bitcoind.{DeriveAddressesResult, LabelResult}
import org.bitcoins.commons.serializers.JsonReaders.LabelPurposeReads
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, BitcoindVersion, DescriptorRpc}
import org.bitcoins.rpc.client.v17.{BitcoindV17RpcClient, V17LabelRpc}
import org.bitcoins.rpc.config.BitcoindInstance
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.concurrent.Future

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

  override def version: BitcoindVersion = BitcoindVersion.Unknown
}
