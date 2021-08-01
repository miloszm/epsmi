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
package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.typesafe.config.Config
import grizzled.slf4j.Logging

case class RescanConfig(rescan: Boolean, startBlock: Int)

object RescanConfig {

  def init(config: Config): RescanConfig = {
    RescanConfig(config.getBoolean("epsmi.rescan"), config.getInt("epsmi.rescan-start-block"))
  }
}

object ReScanner extends Logging {

  def rescan(rpcCli: BitcoindRpcExtendedClient, startBlock: Int): Unit = {
    logger.info("Rescanning... see the bitcoin node's debug.log file")
    wrap(rpcCli.rescanBlockChain(startBlock))
  }

}
