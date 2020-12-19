package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.typesafe.config.Config
import grizzled.slf4j.{Logger, Logging}

case class RescanConfig (
  rescan: Boolean,
  startBlock: Int
)

object RescanConfig {
  def init(config: Config): RescanConfig = {
    RescanConfig(
      config.getBoolean("epsmi.rescan"),
      config.getInt("epsmi.rescan-start-block")
    )
  }
}

object ReScanner extends Logging {

  def rescan(rpcCli: BitcoindRpcExtendedClient, startBlock: Int): Unit = {
    logger.info("Rescanning. . . for progress indicator see the bitcoin node's debug.log file")
    wrap(rpcCli.rescanBlockChain(startBlock))
  }

}
