package com.mhm.setup

import com.typesafe.config.Config
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.concurrent.Future


object Setup {
  def getScriptPubKeysToMonitor(rpcCli: BitcoindRpcClient with V17LabelRpc, config: Config): Future[Map[BitcoinAddress, LabelResult]] = {
    val m = rpcCli.getAddressesByLabel("electrum-watchonly-addresses")
    m
  }
}
