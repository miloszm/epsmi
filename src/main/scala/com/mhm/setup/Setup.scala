package com.mhm.setup

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.wallet.{AddrsSpks, DeterministicWallet}
import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.SetHasAsScala


case class ScriptPubKeysToMonitorResult(spksToMonitor:Seq[String], wallets:Seq[DeterministicWallet])

object Setup extends Logging {
  def getScriptPubKeysToMonitor(rpcCli: BitcoindRpcClient, rpcCliExt: BitcoindRpcExtendedClient with V17LabelRpc, config: Config): ScriptPubKeysToMonitorResult = {
    val importedAddresses = Await.result(rpcCliExt.getAddressesByLabel("electrum-watchonly-addresses"), 20.seconds).keySet

    val wallets = obtainDeterministicWallets(rpcCli, rpcCliExt, config)

    val TEST_ADDR_COUNT = 3
    logger.info("Displaying first " + TEST_ADDR_COUNT + " addresses of " + "each master public key:")

    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val keyAndWallets = mpks.asScala.toSeq.map{ _.getKey}.zip(wallets)
    val walletsToImport = keyAndWallets.flatMap { case (key, wal) =>
      val AddrsSpks(firstAddrs, firstSpk) = wal.getAddresses(rpcCli, rpcCliExt, 0, 0, TEST_ADDR_COUNT)
      val fromIndex = config.getInt("epsmi.initial-import-count")
      val AddrsSpks(lastAddrs, lastSpk) = wal.getAddresses(rpcCli, rpcCliExt, 0, fromIndex-1, 1)
      if (!(firstAddrs ++ lastAddrs).toSet.subsetOf(importedAddresses.map(_.value))){
        Some(wal)
      } else {
        None
      }
    }

    ScriptPubKeysToMonitorResult(Nil, walletsToImport)
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
