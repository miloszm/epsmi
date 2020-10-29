package com.mhm.setup

import com.typesafe.config.Config
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.commons.serializers.JsonSerializers
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{BitcoindRpcClient, Client}
import org.bitcoins.rpc.client.v16.BitcoindV16RpcClient
import org.bitcoins.rpc.client.v17.{BitcoindV17RpcClient, V17LabelRpc}
import org.bitcoins.rpc.client.v19.BitcoindV19RpcClient
import play.api.libs.json.JsString

import scala.concurrent.Future


//case class GetKeysToMonitorResult (
//  needsImport: Boolean,
//  relevantAddrs: List[String],
//  deterministicWallets: List[String]
//)


object Setup {




//  def getAddressesByLabel(label: String, client: Client): Future[Map[BitcoinAddress, LabelResult]] = {
//    client.bitcoindCall[Map[BitcoinAddress, LabelResult]]("getaddressesbylabel", List(JsString(label)))(JsonSerializers.mapAddressesByLabelReads)
//  }


  def getScriptPubKeysToMonitor(rpcCli: BitcoindV17RpcClient, config: Config): Future[Map[BitcoinAddress, LabelResult]] = {

    val m = rpcCli.getAddressesByLabel("electrum-watchonly-addresses")

    m
  }

}
