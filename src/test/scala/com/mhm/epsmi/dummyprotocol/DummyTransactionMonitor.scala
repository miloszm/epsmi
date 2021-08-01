/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
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
