package com.mhm.api4electrum

import com.mhm.common.model.{BroadcastMethod, OwnNode, UnsupportedBroadcastMethod}
import com.typesafe.config.Config


case class Api4ElectrumCoreConfig(
  enableMempoolFeeHistogram: Boolean,
  broadcastMethod: BroadcastMethod,
  port: Int,
  isTestnet: Boolean,
  btcRpcUsername: String,
  btcRpcPassword: String,
  initialImportCount: Int
)

object Api4ElectrumCoreConfig {
  def getDefault = Api4ElectrumCoreConfig(
    enableMempoolFeeHistogram = false,
    broadcastMethod = OwnNode,
    port = 50002,
    isTestnet = false,
    btcRpcUsername = "foo",
    btcRpcPassword = "bar",
    initialImportCount = 1000
  )
  def init(config: Config): Api4ElectrumCoreConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val (btcRpcUsername, btcRpcPassword) = (config.getString("epsmi.btc-rpc-username"), config.getString("epsmi.btc-rpc-password"))
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    val broadcastMethod = config.getString("epsmi.broadcast-method") match {
      case "own-node" => OwnNode
      case _ => UnsupportedBroadcastMethod
    }
    val enableMempoolFeeHistogram = config.getBoolean("epsmi.enable-mempool-fee-histogram")
    val port = config.getInt(s"epsmi.server-port${if (isTestnet) "-testnet" else ""}")
    Api4ElectrumCoreConfig(
      enableMempoolFeeHistogram,
      broadcastMethod,
      port,
      isTestnet,
      btcRpcUsername,
      btcRpcPassword,
      initialImportCount
    )
  }
}
