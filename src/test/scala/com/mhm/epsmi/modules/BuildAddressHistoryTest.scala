package com.mhm.epsmi.modules

import com.mhm.bitcoin.{AddressHistory, TransactionMonitor}
import com.mhm.connectors.BitcoinSConnector.rpcCli
import com.mhm.epsmi.dummy.{DummyBtcRpc, DummyTxCreator}
import com.mhm.util.HashOps
import com.mhm.wallet.{SingleSigWallet, XpubDescTempl}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{be, convertToAnyShouldWrapper}
import scodec.bits.HexStringSyntax

class BuildAddressHistoryTest extends FlatSpec {

  def assertAddressHistoryTx(addressHistory: AddressHistory, spk: String, height: Int, txId: String, subscribed: Boolean): Unit = {
    val historyElement = addressHistory.m.getOrElse(HashOps.script2ScriptHash(spk), fail)
    historyElement.history.head.height shouldBe height
    historyElement.history.head.txHash shouldBe txId
    if (height == 0)
      historyElement.history.head.fee shouldBe 0
    historyElement.subscribed shouldBe subscribed
  }

  "transaction monitor" should "build single entry address history" in {
    val args = XpubDescTempl("xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj", "pkh({xpub}/{change}/*)")
    val singleSigWallet = new SingleSigWallet(rpcCli, hex"0488b21e", args, gapLimit = 25, "wallet-1")
    val deterministicWallets = Seq(singleSigWallet)

    val (dummySpk, blockHeight, dummyTx) = DummyTxCreator.createDummyFundingTx()
    val rpc = new DummyBtcRpc(Seq(dummyTx), Nil, Map(dummyTx.blockhash -> blockHeight))
    val monitor = new TransactionMonitor(rpc, false)
    val addressHistory = monitor.buildAddressHistory(Seq(dummySpk), deterministicWallets)
    addressHistory.m.size shouldBe 1
    assertAddressHistoryTx(addressHistory, dummySpk, blockHeight, dummyTx.txId, subscribed = false)
  }
}
