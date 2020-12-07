package com.mhm.main

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
    logger.info("started getScriptPubKeysToMonitor")
    val importedAddresses = wrap(rpcCli.getAddressesByLabel("electrum-watchonly-addresses"), "getAddressesByLabel").keySet
    logger.debug(s"imported ${importedAddresses.size} addresses, head is ${importedAddresses.headOption}")

    val deterministicWallets = obtainDeterministicWallets(rpcCli, config)

    val TEST_ADDR_COUNT = 3
    logger.info(s"Displaying first $TEST_ADDR_COUNT addresses of each master public key:")

    val initialImportCount = config.getInt("epsmi.initial-import-count")

    val isTestnet = config.getBoolean("epsmi.testnet")
    val mpkConfig = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
    val mpks = mpkConfig.entrySet()
    val keyAndWallets = mpks.asScala.toSeq.map{ _.getKey}.zip(deterministicWallets)
    val walletsToImport = keyAndWallets.flatMap { case (key, wal) =>
      logger.debug(s"getting $TEST_ADDR_COUNT addresses for wallet: $key")
      val AddrsSpks(firstAddrs, firstSpk) = wal.getAddresses(rpcCli, 0, 0, count = TEST_ADDR_COUNT)
      firstAddrs.foreach{ addr => logger.info(s"        $addr") }
      val fromIndex = initialImportCount
      logger.debug(s"getting 1 address for wallet: $key from index ${fromIndex-1}")
      val AddrsSpks(lastAddrs, lastSpk) = wal.getAddresses(rpcCli, 0, fromIndex-1, 1)
      if (!(firstAddrs ++ lastAddrs).toSet.subsetOf(importedAddresses.map(_.value))){
        Some(wal)
      } else {
        None
      }
    }
    val importNeeded = walletsToImport.nonEmpty
    logger.debug(s"import needed is set to $importNeeded")
    val result = if (importNeeded){
      ScriptPubKeysToMonitorResult(importNeeded, Nil, walletsToImport)
    } else {
      val spksToMonitor = (for{
        wal <- deterministicWallets
        change <- 0 to 1
      } yield {
        logger.debug(s"getting $initialImportCount addresses from index 0 with change=$change")
        val AddrsSpks(addrs, spks) = wal.getAddresses(rpcCli, change, 0, initialImportCount)
        spks
      }).flatten
      ScriptPubKeysToMonitorResult(false, spksToMonitor, deterministicWallets)
    }
    logger.info(s"finished getScriptPubKeysToMonitor with ${result.spksToMonitor.size} ScriptPubKeys to monitor from ${result.wallets.size} wallet(s)")
    result
  }

  def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
    }

  def obtainDeterministicWallets(rpcCli: BlockchainRpc with DescriptorRpc, config: Config): Seq[DeterministicWallet] = {
    logger.info("obtaining deterministic wallets")
    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    val chain = networkString(wrap(rpcCli.getBlockChainInfo, "getBlockChainInfo").chain)
    logger.info(s"chain is: $chain")
    val wallets = mpks.asScala.map { mpkEntry =>
      val mpk = mpkEntry.getValue.unwrapped().toString
      val gapLimit = config.getInt("epsmi.gap-limit")
      val wallet = DeterministicWallet.parseElectrumMasterPublicKey(rpcCli, mpk, gapLimit, chain, mpkEntry.getKey)
      wallet.asInstanceOf[DeterministicWallet]
    }
    logger.info(s"obtained ${wallets.size} deterministic wallet(s), head is ${wallets.headOption.map(w => w.walletName).getOrElse("unknown")}")
    wallets.toSeq
  }
}
