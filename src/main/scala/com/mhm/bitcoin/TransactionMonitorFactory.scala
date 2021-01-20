package com.mhm.bitcoin

import com.mhm.connectors.BitcoindRpcExtendedClient

object TransactionMonitorFactory {

  def create(rpcCli: BitcoindRpcExtendedClient): TransactionMonitor =
    new TransactionMonitorImpl(rpcCli: BitcoindRpcExtendedClient, nonWalletAllowed = false)

}
