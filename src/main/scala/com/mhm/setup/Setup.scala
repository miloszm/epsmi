package com.mhm.setup

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.wallet.{AddrsSpks, DeterministicWallet}
import com.typesafe.config.Config
import grizzled.slf4j.Logging
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.rpc.client.common.{BlockchainRpc, DescriptorRpc, UtilRpc}
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.jdk.CollectionConverters.SetHasAsScala


case class ScriptPubKeysToMonitorResult(importNeeded: Boolean, spksToMonitor:Seq[String], wallets:Seq[DeterministicWallet])



class Setup(rpcCli: BitcoindRpcExtendedClient, config: Config) extends Logging {

  type SetupRpc = BlockchainRpc with DescriptorRpc with V17LabelRpc with UtilRpc

  def getScriptPubKeysToMonitor(): ScriptPubKeysToMonitorResult = {
    val importedAddresses = wrap(rpcCli.getAddressesByLabel("electrum-watchonly-addresses"), "getAddressesByLabel 1").keySet

    val deterministicWallets = obtainDeterministicWallets(rpcCli, config)

    val TEST_ADDR_COUNT = 3
    logger.info("Displaying first " + TEST_ADDR_COUNT + " addresses of " + "each master public key:")

    val initialImportCount = config.getInt("epsmi.initial-import-count")

    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val keyAndWallets = mpks.asScala.toSeq.map{ _.getKey}.zip(deterministicWallets)
    val walletsToImport = keyAndWallets.flatMap { case (key, wal) =>
      val AddrsSpks(firstAddrs, firstSpk) = wal.getAddresses(rpcCli, 0, 0, TEST_ADDR_COUNT)
      val fromIndex = initialImportCount
      val AddrsSpks(lastAddrs, lastSpk) = wal.getAddresses(rpcCli, 0, fromIndex-1, 1)
      if (!(firstAddrs ++ lastAddrs).toSet.subsetOf(importedAddresses.map(_.value))){
        Some(wal)
      } else {
        None
      }
    }
    val importNeeded = walletsToImport.nonEmpty
    if (importNeeded){
      ScriptPubKeysToMonitorResult(importNeeded, Nil, walletsToImport)
    } else {
      val spksToMonitor = (for{
        wal <- deterministicWallets
        change <- 0 to 1
      } yield {
        val AddrsSpks(addrs, spks) = wal.getAddresses(rpcCli, change, 0, initialImportCount)
        spks
      }).flatten
      ScriptPubKeysToMonitorResult(false, spksToMonitor, deterministicWallets)
    }
  }

  def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
    }

  def obtainDeterministicWallets(rpcCli: BlockchainRpc with DescriptorRpc, config: Config): Seq[DeterministicWallet] = {
    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val chain = networkString(wrap(rpcCli.getBlockChainInfo, "getBlockChainInfo 0").chain)
    val wallets = mpks.asScala.map { mpkEntry =>
      val mpk = mpkEntry.getValue.unwrapped().toString
      val gapLimit = config.getInt("epsmi.gap-limit")
      val wallet = DeterministicWallet.parseElectrumMasterPublicKey(rpcCli, mpk, gapLimit, chain)
      wallet.asInstanceOf[DeterministicWallet]
    }
    wallets.toSeq
  }
}
