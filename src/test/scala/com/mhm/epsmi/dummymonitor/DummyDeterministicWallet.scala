package com.mhm.epsmi.dummymonitor

import com.mhm.wallet.DeterministicWallet
import org.bitcoins.rpc.client.common.DescriptorRpc

class DummyDeterministicWallet extends DeterministicWallet(gapLimit = 25, walletName = "dummy") {

  override def haveScriptpubkeysOverrunGaplimit(scriptpubkeys: Seq[String]): Map[Int, Int] = Map()

  override def deriveAddresses(rpcCli: DescriptorRpc, change: Int, fromIndex: Int, count: Int): Seq[String] = ???

}
