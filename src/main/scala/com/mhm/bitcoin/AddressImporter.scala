package com.mhm.bitcoin

import com.mhm.connectors.RpcWrap.wrap
import com.mhm.connectors.{BitcoindRpcExtendedClient, ImportMultiRequestV18}
import com.mhm.main.Constants.ADDRESSES_LABEL
import com.mhm.wallet.{DescriptorDeterministicWallet, DeterministicWallet}
import grizzled.slf4j.Logging
import org.bitcoins.commons.jsonmodels.bitcoind.RpcOpts.{ImportMultiAddress, ImportMultiRequest}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress

object AddressImporter extends Logging {

  def importAddresses(rpcCli: BitcoindRpcExtendedClient,
                      watchonlyAddresses: Seq[String],
                      wallets: Seq[DeterministicWallet],
                      changeParam: Int, // 0 for receive, 1 for change, -1 for both
                      count: Int): Unit = {
    logger.debug(
      s"Importing ${watchonlyAddresses.size} watch-only address(es) and ${wallets.size} wallets(s) into label ${ADDRESSES_LABEL}"
    )
    watchonlyAddresses.foreach { address =>
      logger.info(s"converting address $address to BitcoinAddress")
      val importAddress = BitcoinAddress.fromString(address)
      wrap(
        rpcCli.importMulti(
          Vector(
            ImportMultiRequest(
              scriptPubKey = ImportMultiAddress(importAddress),
              timestamp    = UInt32(System.currentTimeMillis() / 1000),
              watchonly    = Some(true),
              label        = Some(ADDRESSES_LABEL),
            )
          ),
          rescan = false
        )
      )
    }

    wallets.zipWithIndex.foreach {
      case (wallet, i) =>
        logger.info(s"Importing wallet $i/${wallets.size}")
        wallet match {
          case wallet1: DescriptorDeterministicWallet =>
            if (changeParam == 0 || changeParam == -1) {
              wrap(
                rpcCli.importMultiV18(
                  Vector(
                    ImportMultiRequestV18(
                      desc      = wallet1.descriptors(0),
                      range     = Array(0, count - 1),
                      label     = Some(ADDRESSES_LABEL),
                      watchonly = Some(true),
                      timestamp = UInt32(System.currentTimeMillis() / 1000)
                    )
                  ),
                  rescan = false
                )
              )
            }
            if (changeParam == 1 || changeParam == -1) {
              wrap(
                rpcCli.importMultiV18(
                  Vector(
                    ImportMultiRequestV18(
                      desc      = wallet1.descriptors(1),
                      range     = Array(0, count - 1),
                      label     = Some(ADDRESSES_LABEL),
                      watchonly = Some(true),
                      timestamp = UInt32(System.currentTimeMillis() / 1000)
                    )
                  ),
                  rescan = false
                )
              )
            }
          case _ =>
        }
    }

    logger.debug("Importing done")
    ()
  }

}
