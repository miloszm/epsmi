package com.mhm.setup

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.wallet.DeterministicWallet
import com.typesafe.config.Config
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.SetHasAsScala

object Setup {
  def getScriptPubKeysToMonitor(rpcCliExt: BitcoindRpcExtendedClient with V17LabelRpc, config: Config): Future[Map[BitcoinAddress, LabelResult]] = {
    val m = rpcCliExt.getAddressesByLabel("electrum-watchonly-addresses")
    m
  }
  def obtainDeterministicWallets(rpcCli: BitcoindRpcClient, config: Config): Seq[DeterministicWallet] = {
    val mpkConfig = config.getConfig("epsmi.masterpublickeys")
    val mpks = mpkConfig.entrySet()
    val chain = Await.result(rpcCli.getBlockChainInfo, 20.seconds).chain
    mpks.asScala.foreach{ mpkEntry =>
      val mpk = mpkEntry.getValue.render()
      val gapLimit = config.getInt("epsmi.gap-limit")
    }

    Nil
  }
}
