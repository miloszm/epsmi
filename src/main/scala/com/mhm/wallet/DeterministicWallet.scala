package com.mhm.wallet

import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.rpc.client.common.BitcoindRpcClient


case class XpubDescTempl(xpub: String, descTempl: String)

abstract class DeterministicWallet(rpc: Any)

abstract class DescriptorDeterministicWallet(rpc: Any, xpubVbytes: Array[Byte], args: XpubDescTempl) extends DeterministicWallet(rpc) {
  def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Any
}

class SingleSigWallet(rpc: Any, xpubVbytes: Array[Byte], args: XpubDescTempl) extends DescriptorDeterministicWallet(rpc, xpubVbytes, args){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Any = ???
}

class MultisigWallet(rpc: Any, xpubVbytes: Array[Byte], args: XpubDescTempl) extends DescriptorDeterministicWallet(rpc, xpubVbytes, args){
  override def obtainDescriptorsWithoutChecksum(args: XpubDescTempl): Any = ???
}

class SingleSigOldMnemonicWallet extends DeterministicWallet

object DeterministicWallet {
  def parseElectrumMasterPublicKey(mpk: String, gapLimit: Int, rpcCli: BitcoindRpcClient, chain: NetworkParameters): Unit = {
    val xpubVvytes: Array[Byte] = if (chain.name == "main")
      Array(0x04, 0x88, 0xb2, 0x1e).map(_.toByte)
    else if (chain.name == "test" || chain.name == "regtest")
      Array(0x04, 0x35, 0x87, 0xcf).map(_.toByte)
    else
      throw new IllegalStateException("unrecognized bitcoin chain")

  }
}
