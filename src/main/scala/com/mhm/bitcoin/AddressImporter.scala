package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient
import com.mhm.main.Constants
import com.mhm.wallet.{DescriptorDeterministicWallet, DeterministicWallet}
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.number.UInt32

object AddressImporter extends Logging {

  def importAddresses(
    rpcCli: BitcoindRpcExtendedClient,
    watchonlyAddresses: Seq[String],
    wallets: Seq[DeterministicWallet],
    changeParam: Int, // 0 for receive, 1 for change, -1 for both
    count: Int
  ): Unit = {
    logger.debug(s"Importing ${watchonlyAddresses.size} watch-only address(es) and ${wallets.size} wallets(s) into label ${Constants.ADDRESSES_LABEL}")
    val importMultiRequest = watchonlyAddresses.map { wa =>
      RpcOpts.ImportMultiRequest(
        scriptPubKey = RpcOpts.ImportMultiAddress(BitcoinAddress.fromString(wa)),
        timestamp = UInt32(System.currentTimeMillis()), // TODO refactor
        label = Some(Constants.ADDRESSES_LABEL)
      )
    }.toVector
    rpcCli.importMulti(importMultiRequest, rescan = false)
    wallets.zipWithIndex.foreach { case (wallet, i) =>
      logger.info(s"Importing wallet $i/${wallets.size}")
      if (wallet.isInstanceOf[DescriptorDeterministicWallet]){
        if (changeParam == 0 || changeParam == -1){

        }
        if (changeParam == 1 || changeParam == -1){

        }
      }
    }

    ()
  }

}
