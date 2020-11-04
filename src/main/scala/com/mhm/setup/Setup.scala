package com.mhm.setup

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.wallet.DeterministicWallet
import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.LabelResult
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters.SetHasAsScala


case class ScriptPubKeysToMonitorResult(spksToMonitor:Seq[String], wallets:Seq[DeterministicWallet])

object Setup extends Logging {
  def getScriptPubKeysToMonitor(rpcCli: BitcoindRpcClient, rpcCliExt: BitcoindRpcExtendedClient with V17LabelRpc, config: Config): ScriptPubKeysToMonitorResult = {
//    val m = rpcCliExt.getAddressesByLabel("electrum-watchonly-addresses")
//    m
    val wallets = obtainDeterministicWallets(rpcCli, rpcCliExt, config)

    val TEST_ADDR_COUNT = 3
    logger.info("Displaying first " + TEST_ADDR_COUNT + " addresses of " + "each master public key:")

    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val keyAndWallets = mpks.asScala.map{ _.getKey}.zip(wallets)
//    val walletsToImport = keyAndWallets.map { case (key, wal) =>
//      wal.getAddresses(0, 0, TEST_ADDR_COUNT)
//    }

    ScriptPubKeysToMonitorResult(Nil, Nil)
  }

  def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
    }

  def obtainDeterministicWallets(rpcCli: BitcoindRpcClient, rpcCliExt: BitcoindRpcExtendedClient, config: Config): Seq[DeterministicWallet] = {
    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val chain = networkString(Await.result(rpcCli.getBlockChainInfo, 20.seconds).chain)
    val wallets = mpks.asScala.map { mpkEntry =>
      val mpk = mpkEntry.getValue.unwrapped().toString
      val gapLimit = config.getInt("epsmi.gap-limit")
      val wallet = DeterministicWallet.parseElectrumMasterPublicKey(rpcCliExt, mpk, gapLimit, chain)
      wallet.asInstanceOf[DeterministicWallet]
    }
    wallets.toSeq
  }
}
