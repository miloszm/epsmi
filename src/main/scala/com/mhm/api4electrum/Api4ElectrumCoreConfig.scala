/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.api4electrum

import com.mhm.common.model.{BroadcastMethod, OwnNode, UnsupportedBroadcastMethod}
import com.typesafe.config.Config

case class Api4ElectrumCoreConfig(enableMempoolFeeHistogram: Boolean,
                                  broadcastMethod: BroadcastMethod,
                                  port: Int,
                                  isTestnet: Boolean,
                                  btcRpcUsername: String,
                                  btcRpcPassword: String,
                                  initialImportCount: Int)

object Api4ElectrumCoreConfig {

  def getDefault = Api4ElectrumCoreConfig(
    enableMempoolFeeHistogram = false,
    broadcastMethod           = OwnNode,
    port                      = 50002,
    isTestnet                 = false,
    btcRpcUsername            = "foo",
    btcRpcPassword            = "bar",
    initialImportCount        = 1000
  )

  def init(config: Config): Api4ElectrumCoreConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val (btcRpcUsername, btcRpcPassword) =
      (config.getString("epsmi.btc-rpc-username"), config.getString("epsmi.btc-rpc-password"))
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    val broadcastMethod = config.getString("epsmi.broadcast-method") match {
      case "own-node" => OwnNode
      case _          => UnsupportedBroadcastMethod
    }
    val enableMempoolFeeHistogram =
      config.getBoolean("epsmi.enable-mempool-fee-histogram")
    val port =
      config.getInt(s"epsmi.server-port${if (isTestnet) "-testnet" else ""}")
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
