package com.mhm.setup

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.typesafe.config.Config
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.concurrent.Future

object Setup {
  def getScriptPubKeysToMonitor(rpcCliExt: BitcoindRpcExtendedClient with V17LabelRpc, config: Config): Future[Map[BitcoinAddress, LabelResult]] = {
    val m = rpcCliExt.getAddressesByLabel("electrum-watchonly-addresses")
    m
  }
}
