package com.mhm.main

import java.util
import java.util.Map

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.HashOps
import com.mhm.wallet.{AddrsSpks, DeterministicWallet}
import com.typesafe.config.{Config, ConfigValue}
import grizzled.slf4j.Logging
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{BlockchainRpc, DescriptorRpc, UtilRpc}
import org.bitcoins.rpc.client.v17.V17LabelRpc

import scala.jdk.CollectionConverters.SetHasAsScala


case class ScriptPubKeysToMonitorResult(importNeeded: Boolean, spksToMonitor:Seq[String], wallets:Seq[DeterministicWallet])

case class SetupConfig(
  isTestnet: Boolean,
  mpks: Seq[util.Map.Entry[String, ConfigValue]],
  initialImportCount: Int
)

object SetupConfig {
  def init(config: Config): SetupConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val mpkConfig = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
    val mpks = mpkConfig.entrySet().asScala.toSeq
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    SetupConfig(isTestnet, mpks, initialImportCount)
  }
}

class Setup(rpcCli: BitcoindRpcExtendedClient, config: Config) extends Logging {

  val TEST_ADDR_COUNT = 3
  type SetupRpc = BlockchainRpc with DescriptorRpc with V17LabelRpc with UtilRpc

  def getScriptPubKeysToMonitor(): ScriptPubKeysToMonitorResult = {
    logger.trace("started getScriptPubKeysToMonitor")
    val listedLabels = wrap(rpcCli.listLabels())
    val importedAddresses = if (listedLabels.contains(Constants.ADDRESSES_LABEL)) {
      wrap(rpcCli.getAddressesByLabel("electrum-watchonly-addresses"), "getAddressesByLabel").keySet
    } else Set[BitcoinAddress]()
    importedAddresses.foreach { ia => logger.trace(s"imported addr = ${ia.value}") }
    logger.debug(s"imported ${importedAddresses.size} addresses, head is ${importedAddresses.headOption}")
    val setupConfig = SetupConfig.init(config)

    val deterministicWallets = obtainDeterministicWallets(rpcCli, setupConfig.mpks)
    val walletsToImport = determineWalletsToImport(deterministicWallets, setupConfig.mpks, setupConfig.initialImportCount, importedAddresses)
    val importNeeded = walletsToImport.nonEmpty
    logger.debug(s"import needed is set to $importNeeded")
    val result = if (importNeeded){
      logger.info(s"Importing ${walletsToImport.size} wallets into the Bitcoin node")
      ScriptPubKeysToMonitorResult(importNeeded, Nil, walletsToImport)
    } else {
      val spksToMonitor = determineScriptPubKeysToMonitor(importedAddresses, deterministicWallets, setupConfig.initialImportCount)
      ScriptPubKeysToMonitorResult(importNeeded = false, spksToMonitor, deterministicWallets)
    }
    logger.trace(s"finished getScriptPubKeysToMonitor with ${result.spksToMonitor.size} ScriptPubKeys to monitor from ${result.wallets.size} wallet(s)")
    result
  }

  private def determineScriptPubKeysToMonitor(importedAddresses: Set[BitcoinAddress], deterministicWallets: Seq[DeterministicWallet], initialImportCount: Int): Seq[String] = {
    (for {
      wal <- deterministicWallets
      change <- 0 to 1
    } yield {
      logger.debug(s"getting $initialImportCount addresses from index 0 with change=$change")
      val AddrsSpks(addrs, spks) = wal.getAddresses(rpcCli, change, 0, initialImportCount)
      addrs.zip(spks).foreach { case (addr, spk) =>
        logger.trace(s"addr = $addr  <==>  spk = $spk  <==>  sh = ${HashOps.script2ScriptHash(spk)}")
      }
      val firstNotImported = wal.findFirstNotImported(rpcCli, change, importedAddresses)
      logger.trace(s"found first not imported: $firstNotImported")
      wal.rewindOne(change)
      spks :+ firstNotImported
    }).flatten
  }

  private def determineWalletsToImport(
    deterministicWallets: Seq[DeterministicWallet],
    mpks: Seq[util.Map.Entry[String, ConfigValue]],
    initialImportCount: Int,
    importedAddresses: Set[BitcoinAddress]
  ): Seq[DeterministicWallet] = {
    val keyAndWallets = mpks.map {_.getKey}.zip(deterministicWallets)
    logger.info(s"Displaying first $TEST_ADDR_COUNT addresses of each master public key:")
    val walletsToImport = keyAndWallets.flatMap { case (key, wal) =>
      logger.debug(s"getting $TEST_ADDR_COUNT addresses for wallet: $key")
      val AddrsSpks(firstAddrs, _) = wal.getAddresses(rpcCli, 0, 0, count = TEST_ADDR_COUNT)
      firstAddrs.foreach { addr => logger.info(s"        $addr") }
      val fromIndex = initialImportCount
      logger.debug(s"getting 1 address for wallet: $key from index ${fromIndex - 1}")
      val AddrsSpks(lastAddrs, _) = wal.getAddresses(rpcCli, 0, fromIndex - 1, 1)
      if (!(firstAddrs ++ lastAddrs).toSet.subsetOf(importedAddresses.map(_.value))) {
        Some(wal)
      } else {
        None
      }
    }
    walletsToImport
  }

  def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
    }

  def obtainDeterministicWallets(rpcCli: BlockchainRpc with DescriptorRpc, mpks: Seq[util.Map.Entry[String, ConfigValue]]): Seq[DeterministicWallet] = {
    logger.info("obtaining deterministic wallets")
    val chain = networkString(wrap(rpcCli.getBlockChainInfo, "getBlockChainInfo").chain)
    logger.info(s"chain is: $chain")
    val wallets = mpks.map { mpkEntry =>
      val mpk = mpkEntry.getValue.unwrapped().toString
      val gapLimit = config.getInt("epsmi.gap-limit")
      val wallet = DeterministicWallet.parseElectrumMasterPublicKey(rpcCli, mpk, gapLimit, chain, mpkEntry.getKey)
      wallet.asInstanceOf[DeterministicWallet]
    }
    logger.info(s"obtained ${wallets.size} deterministic wallet(s), head is ${wallets.headOption.map(w => w.walletName).getOrElse("unknown")}")
    wallets
  }
}
