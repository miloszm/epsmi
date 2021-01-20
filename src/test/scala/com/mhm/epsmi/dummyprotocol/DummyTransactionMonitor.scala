package com.mhm.epsmi.dummyprotocol

import com.mhm.bitcoin.{AddressBalance, TransactionMonitor, TransactionMonitorState, Tx4HistoryGen}
import com.mhm.common.model.HistoryElement
import com.mhm.wallet.DeterministicWallet
import org.bitcoins.commons.jsonmodels.bitcoind.RpcTransaction
import org.bitcoins.crypto.DoubleSha256DigestBE

class DummyTransactionMonitor extends TransactionMonitor {
  override def buildAddressHistory(monitoredScriptPubKeys: Seq[String],
                                   deterministicWallets: Seq[DeterministicWallet]): TransactionMonitorState = ???
  override def checkForUpdatedTxs(state: TransactionMonitorState): (Set[String], TransactionMonitorState)   = ???
  override def getAddressBalance(state: TransactionMonitorState, sh: String): Option[AddressBalance]        = ???
}
