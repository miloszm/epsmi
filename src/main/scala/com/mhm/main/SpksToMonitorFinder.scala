package com.mhm.main

import java.util

import com.mhm.bitcoin.{NoopWalletStateListener, WalletStateListener}
import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.util.HashOps
import com.mhm.wallet.{AddrsSpksPair, DeterministicWallet}
import com.typesafe.config.{Config, ConfigValue}
import grizzled.slf4j.Logging
import org.bitcoins.core.config.{MainNet, NetworkParameters, RegTest, TestNet3}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.client.common.{BlockchainRpc, DescriptorRpc, UtilRpc}
import org.bitcoins.rpc.client.v17.V17LabelRpc

class SpksToMonitorFinder(rpcCli: BitcoindRpcExtendedClient, config: Config) extends Logging {

  val TEST_ADDR_COUNT = 3
  type SetupRpc = BlockchainRpc with DescriptorRpc with V17LabelRpc with UtilRpc

  def getScriptPubKeysToMonitor(
    walletStateListener: WalletStateListener = NoopWalletStateListener
  ): SpksToMonitorResult = {
    logger.trace("started getScriptPubKeysToMonitor")
    val listedLabels = wrap(rpcCli.listLabels())
    val importedAddresses = if (listedLabels.contains(Constants.ADDRESSES_LABEL)) {
      wrap(rpcCli.getAddressesByLabel("electrum-watchonly-addresses"), "getAddressesByLabel").keySet
    } else Set[BitcoinAddress]()
    importedAddresses.foreach { ia =>
      logger.trace(s"imported addr = ${ia.value}")
    }
    logger.debug(s"imported ${importedAddresses.size} addresses, head is ${importedAddresses.headOption}")
    val setupConfig = SpksToMonitorFinderConfig.init(config)

    val deterministicWallets =
      obtainDeterministicWallets(rpcCli, setupConfig.mpks, walletStateListener)
    val walletsToImport = determineWalletsToImport(
      deterministicWallets,
      setupConfig.mpks,
      setupConfig.initialImportCount,
      importedAddresses
    )
    val importNeededForWallets = walletsToImport.nonEmpty

    val watchOnlyAddresses                = setupConfig.woas.map(_.getValue.unwrapped().toString()).toSet
    val importedAddressesStrings          = importedAddresses.map(_.value)
    val importNeededForWatchOnlyAddresses = !watchOnlyAddresses.subsetOf(importedAddressesStrings)
    val watchOnlyAddressesToImport        = watchOnlyAddresses.diff(importedAddressesStrings).toSeq

    val importNeeded = importNeededForWallets || importNeededForWatchOnlyAddresses
    logger.debug(s"import needed is set to $importNeeded")

    val result = if (importNeeded) {
      logger.info(
        s"Importing ${walletsToImport.size} wallets and ${watchOnlyAddressesToImport.size} watch-only addresses into the Bitcoin node"
      )
      SpksToMonitorResult(importNeeded, watchOnlyAddressesToImport, walletsToImport)
    } else {
      val spksToMonitor =
        determineScriptPubKeysToMonitor(importedAddresses, deterministicWallets, setupConfig.initialImportCount)
      SpksToMonitorResult(importNeeded = false, spksToMonitor, deterministicWallets)
    }
    logger.trace(
      s"finished getScriptPubKeysToMonitor with ${result.spksToMonitor.size} ScriptPubKeys to monitor from ${result.wallets.size} wallet(s)"
    )
    result
  }

  private def determineScriptPubKeysToMonitor(importedAddresses: Set[BitcoinAddress],
                                              deterministicWallets: Seq[DeterministicWallet],
                                              initialImportCount: Int): Seq[String] = {
    (for {
      wal    <- deterministicWallets
      change <- 0 to 1
    } yield {
      logger.debug(s"getting $initialImportCount addresses from index 0 with change=$change")
      val AddrsSpksPair(addrs, spks) = wal.getAddresses(rpcCli, change, 0, initialImportCount)
      addrs.zip(spks).foreach {
        case (addr, spk) =>
          logger.trace(s"addr = $addr  <==>  spk = $spk  <==>  sh = ${HashOps.script2ScriptHash(spk)}")
      }
      val firstNotImported = wal.findFirstNotImported(rpcCli, change, importedAddresses)
      logger.trace(s"found first not imported: $firstNotImported")
      wal.rewindOne(change)
      spks :+ firstNotImported
    }).flatten
  }

  private def determineWalletsToImport(deterministicWallets: Seq[DeterministicWallet],
                                       mpks: Seq[util.Map.Entry[String, ConfigValue]],
                                       initialImportCount: Int,
                                       importedAddresses: Set[BitcoinAddress]): Seq[DeterministicWallet] = {
    val keyAndWallets = mpks.map { _.getKey }.zip(deterministicWallets)
    logger.info(s"Displaying first $TEST_ADDR_COUNT addresses of each master public key:")
    val walletsToImport = keyAndWallets.flatMap {
      case (key, wal) =>
        logger.debug(s"getting $TEST_ADDR_COUNT addresses for wallet: $key")
        val AddrsSpksPair(firstAddrs, _) = wal.getAddresses(rpcCli, 0, 0, count = TEST_ADDR_COUNT)
        firstAddrs.foreach { addr =>
          logger.info(s"        $addr")
        }
        val fromIndex = initialImportCount
        logger.debug(s"getting 1 address for wallet: $key from index ${fromIndex - 1}")
        val AddrsSpksPair(lastAddrs, _) = wal.getAddresses(rpcCli, 0, fromIndex - 1, 1)
        if (!(firstAddrs ++ lastAddrs).toSet.subsetOf(importedAddresses.map(_.value))) {
          Some(wal)
        } else {
          None
        }
    }
    walletsToImport
  }

  private def networkString(network: NetworkParameters) =
    network match {
      case MainNet  => "main"
      case RegTest  => "regtest"
      case TestNet3 => "test"
    }

  private def obtainDeterministicWallets(
    rpcCli: BlockchainRpc with DescriptorRpc,
    mpks: Seq[util.Map.Entry[String, ConfigValue]],
    walletStateListener: WalletStateListener = NoopWalletStateListener
  ): Seq[DeterministicWallet] = {
    logger.info("obtaining deterministic wallets")
    val chain = networkString(wrap(rpcCli.getBlockChainInfo, "getBlockChainInfo").chain)
    logger.info(s"chain is: $chain")
    val wallets = mpks.map { mpkEntry =>
      val mpk      = mpkEntry.getValue.unwrapped().toString
      val gapLimit = config.getInt("epsmi.gap-limit")
      val wallet = DeterministicWallet
        .parseElectrumMasterPublicKey(rpcCli, mpk, gapLimit, chain, mpkEntry.getKey, walletStateListener)
      wallet.asInstanceOf[DeterministicWallet]
    }
    logger.info(
      s"obtained ${wallets.size} deterministic wallet(s), head is ${wallets.headOption.map(w => w.walletName).getOrElse("unknown")}"
    )
    wallets
  }
}
