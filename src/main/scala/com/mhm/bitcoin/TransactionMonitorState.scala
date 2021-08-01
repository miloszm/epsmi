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
package com.mhm.bitcoin

import com.mhm.common.model.{AddressHistory, HistoryElement, HistoryEntry}
import com.mhm.util.HashOps.getStatusElectrum
import com.mhm.wallet.DeterministicWallet

case class ReorganizableTxEntry(txid: String, blockhashOpt: Option[String], height: Int, matchingShs: Seq[String])
case class UnconfirmedTxEntry(txid: String, matchingShs: Seq[String])
case class TxidAddress(txid: String, address: String)

object TransactionMonitorState {

  def createEmpty(): TransactionMonitorState = {
    TransactionMonitorState(AddressHistory(Map()))
  }
}

case class TransactionMonitorState(addressHistory: AddressHistory,
                                   deterministicWallets: Seq[DeterministicWallet] = Seq(),
                                   unconfirmedTxes: Map[String, Seq[String]]      = Map(),
                                   reorganizableTxes: Seq[ReorganizableTxEntry]   = Seq(),
                                   updatedScripthashes: Seq[String]               = Seq(),
                                   lastKnownTx: Option[TxidAddress]               = None,
                                   subscribedToHeaders: Boolean                   = false) {

  def removeUnconfirmed(removed: Seq[UnconfirmedTxEntry]): TransactionMonitorState = {
    val newUnconfirmedTxes = unconfirmedTxes.collect {
      case e @ (txid, _) if !removed.map(_.txid).contains(txid) => e
    }
    this.copy(unconfirmedTxes = newUnconfirmedTxes)
  }

  def removeReorganizable(removed: ReorganizableTxEntry): TransactionMonitorState = {
    this.copy(
      reorganizableTxes = this.reorganizableTxes.filterNot(r => r.txid == removed.txid && r.height == removed.height)
    )
  }

  def deleteHistoryItemForScripthashes(shs: Seq[String], txid: String): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) =>
        k -> v.copy(history = v.history.filterNot(_.txHash == txid))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def deleteHistoryItems(shs: Seq[String], txid: String, height: Int): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) =>
        k -> v.copy(history = v.history.filterNot(e => e.txHash == txid && e.height == height))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def addHistoryItemForScripthashes(shs: Seq[String], he: HistoryElement): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) =>
        k -> v.copy(history = he +: v.history.filterNot(a => a.txHash == he.txHash && a.height == he.height))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def updateHeightForScripthashes(shs: Seq[String], txid: String, newHeight: Int): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, h) if shs.contains(k) =>
        k -> h.copy(history = h.history.map(e => if (e.txHash == txid) e.copy(height = newHeight) else e))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def addReorganizableTx(reorganizableTxEntry: ReorganizableTxEntry): TransactionMonitorState = {
    this.copy(
      reorganizableTxes = this.reorganizableTxes.filterNot(_.txid == reorganizableTxEntry.txid) :+ reorganizableTxEntry
    )
  }

  def addUpdatedScripthashes(shs: Seq[String]): TransactionMonitorState = {
    this.copy(updatedScripthashes = this.updatedScripthashes ++ shs)
  }

  def addUnconfirmedScripthases(txid: String, shs: Seq[String]): TransactionMonitorState = {
    val newShs             = unconfirmedTxes.getOrElse(txid, Nil).toSet ++ shs.toSet
    val newUnconfirmedTxes = (unconfirmedTxes - txid) + (txid -> newShs.toSeq)
    this.copy(unconfirmedTxes = newUnconfirmedTxes)
  }

  def setLastKnownTx(txidAddress: TxidAddress): TransactionMonitorState = {
    this.copy(lastKnownTx = Some(txidAddress))
  }

  def setLastKnownTx(txidAddressOpt: Option[TxidAddress]): TransactionMonitorState = {
    this.copy(lastKnownTx = txidAddressOpt)
  }

  def resetLastKnownTx(): TransactionMonitorState = {
    this.copy(lastKnownTx = None)
  }

  def sortAddressHistory(): TransactionMonitorState = {
    def sortHistoryEntry(historyEntry: HistoryEntry): HistoryEntry = {
      val (unconfirmedTxs, confirmedTxs) =
        historyEntry.history.partition(_.height <= 0)
      historyEntry.copy(history = confirmedTxs.sortWith(_.height < _.height) ++ unconfirmedTxs)
    }
    val newMap = addressHistory.m.collect {
      case (k, v) => k -> sortHistoryEntry(v)
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def resetUpdatedScripthashes(): TransactionMonitorState = {
    this.copy(updatedScripthashes = Nil)
  }

  def getElectrumHistory(sh: String): Option[Seq[HistoryElement]] = {
    this.addressHistory.m.get(sh).map(_.history)
  }

  def subscribeAddress(sh: String): TransactionMonitorState = {
    val newMap = addressHistory.m.get(sh) match {
      case Some(he) =>
        (addressHistory.m - sh) + (sh -> he.copy(subscribed = true))
      case None => addressHistory.m
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }

  def initUnconfirmedTxs(): TransactionMonitorState = {
    case class TxShPair(tx: String, sh: String)
    val expanded = addressHistory.m.flatMap {
      case (sh, he) =>
        he.history.filter(_.height <= 0).map(_.txHash).map(tx => TxShPair(tx, sh))
    }
    val reduced = expanded.groupBy(_.tx).view.mapValues(_.map(_.sh).toSeq)
    this.copy(unconfirmedTxes = reduced.toMap)
  }

  def getElectrumHistoryHash(sh: String): String = {
    val hashHeights = this.addressHistory.m
      .get(sh)
      .map {
        _.history.map(_.asHashHeight)
      }
      .getOrElse(Nil)
    getStatusElectrum(hashHeights.toList)
  }

  def subscribeToHeaders(subscribe: Boolean): TransactionMonitorState = {
    this.copy(subscribedToHeaders = subscribe)
  }

}
