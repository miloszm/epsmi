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

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}

import com.mhm.wallet.{AddrsSpksPair, ChangeIndexPair, DeterministicWalletState}

import scala.collection.concurrent

trait TxsMonitorStateListener {
  def updated(newState: TransactionMonitorState): Unit
  def heartbeatTick(): Unit
  def updatedShsTick(shs: Seq[String]): Unit
}

object NoopTxsMonitorStateListener extends TxsMonitorStateListener {
  override def updated(newState: TransactionMonitorState): Unit = ()
  override def heartbeatTick(): Unit                            = ()
  override def updatedShsTick(shs: Seq[String]): Unit           = ()
}

trait WalletStateListener {
  def updated(newState: DeterministicWalletState): Unit
  def setAddressSpkMap(addrsSpks: AddrsSpksPair): Unit
}

object NoopWalletStateListener extends WalletStateListener {
  override def updated(newState: DeterministicWalletState): Unit = ()
  override def setAddressSpkMap(addrsSpks: AddrsSpksPair): Unit  = ()
}

trait TxsMonitorMBean {
  val HistoryMaxSize = 1000
  def getAddressHistory: Array[String]
  def getNonEmptyAddressHistory: Array[String]
  def getUnconfirmedTxs: Array[String]
  def getReorganizableTxs: Array[String]
  def getUpdatedShs: Array[String]
  def getLastKnownTx: String
  def getLastKnownAddress: String
  def getSubscribedToHeaders: Boolean
  def getHeartbeatTick: Long
  def getUpdatedShsTick: Long
  def getUpdatedShsHistory: Array[String]
  def getWalletSPKIndexMap: Array[String]
  def getWalletNextIndexMap: Array[String]
  def getAddrSpkMap: Array[String]

  // set methods to allow copying from fields in jconsole
  def setAddressHistory(a: Array[String])         = ()
  def setNonEmptyAddressHistory(a: Array[String]) = ()
  def setUnconfirmedTxs(a: Array[String])         = ()
  def setReorganizableTxs(a: Array[String])       = ()
  def setUpdatedShs(a: Array[String])             = ()
  def setLastKnownTx(s: String)                   = ()
  def setLastKnownAddress(s: String)              = ()
  def setSubscribedToHeaders(b: Boolean)          = ()
  def setWalletSPKIndexMap(a: Array[String])      = ()
  def setWalletNextIndexMap(a: Array[String])     = ()
  def setAddrSpkMap(a: Array[String])             = ()
}

class TxsMonitor extends TxsMonitorMBean with TxsMonitorStateListener with WalletStateListener {
  val currentMonitorState                             = new AtomicReference[TransactionMonitorState]()
  val currentWalletState                              = new AtomicReference[DeterministicWalletState]()
  val heartbeatCounter                                = new AtomicLong()
  val updatedShsCounter                               = new AtomicLong()
  val updatedShsHistory: concurrent.Map[String, Long] = concurrent.TrieMap.empty
  val addrsSpksMap: concurrent.Map[String, String]    = concurrent.TrieMap.empty
  override def getAddressHistory: Array[String] = {
    currentMonitorState.get.addressHistory.m.collect {
      case (sh, historyEntry) =>
        s"sh=$sh -> ${historyEntry.history.map(he => s"txid=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getNonEmptyAddressHistory: Array[String] = {
    currentMonitorState.get.addressHistory.m.collect {
      case (sh, historyEntry) if historyEntry.history.nonEmpty =>
        s"sh=$sh -> ${historyEntry.history.map(he => s"txid=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getUnconfirmedTxs: Array[String] = {
    currentMonitorState.get.unconfirmedTxes.map {
      case (sh, txids) =>
        s"txid=$sh -> shs=${txids.mkString("|")}"
    }.toArray
  }
  override def getReorganizableTxs: Array[String] = {
    currentMonitorState.get.reorganizableTxes.map { entry =>
      s"txid=${entry.txid} height=${entry.height} blockhash=${entry.blockhashOpt.getOrElse("n/a")} shs=${entry.matchingShs
        .mkString("|")}"
    }.toArray
  }
  override def getUpdatedShs: Array[String] = {
    currentMonitorState.get.updatedScripthashes.toArray
  }
  override def getLastKnownTx: String = {
    currentMonitorState.get.lastKnownTx.map { _.txid }.getOrElse("n/a")
  }
  override def getLastKnownAddress: String = {
    currentMonitorState.get.lastKnownTx.map { _.address }.getOrElse("n/a")
  }
  override def getSubscribedToHeaders: Boolean = {
    currentMonitorState.get().subscribedToHeaders
  }
  override def updated(newState: TransactionMonitorState): Unit = {
    currentMonitorState.set(newState)
  }
  override def heartbeatTick(): Unit = {
    heartbeatCounter.incrementAndGet()
  }
  override def getHeartbeatTick: Long = {
    heartbeatCounter.get()
  }
  override def updatedShsTick(shs: Seq[String]): Unit = {
    if (updatedShsCounter.addAndGet(shs.size) > HistoryMaxSize) {
      updatedShsHistory.clear()
    }
    shs.foreach { updatedShsHistory.put(_, System.currentTimeMillis()) }
  }
  override def getUpdatedShsTick: Long = {
    updatedShsCounter.get()
  }
  override def getUpdatedShsHistory: Array[String] = {
    updatedShsHistory.collect { case (k, v) => s"$v $k" }.toArray
  }
  override def updated(newState: DeterministicWalletState): Unit = {
    currentWalletState.set(newState)
  }
  override def getWalletSPKIndexMap: Array[String] = {
    def sortingFun(p1: (String, ChangeIndexPair), p2: (String, ChangeIndexPair)): Boolean = {
      val (_, l) = p1
      val (_, r) = p2
      if (l.change == r.change) l.index < r.index else l.change < r.change
    }
    currentWalletState.get.scriptPubKeyIndex.toList
      .sortWith(sortingFun)
      .map {
        case (k, v) =>
          s"$k -> change=${v.change} index=${v.index}"
      }
      .toArray
  }
  override def getWalletNextIndexMap: Array[String] = {
    currentWalletState.get.nextIndex.map {
      case (k, v) =>
        s"$k -> $v"
    }.toArray
  }
  override def setAddressSpkMap(addrsSpks: AddrsSpksPair): Unit = {
    addrsSpks.addrs.indices.foreach { i =>
      addrsSpksMap.put(addrsSpks.addrs(i), addrsSpks.spks(i))
    }
  }
  override def getAddrSpkMap: Array[String] = {
    addrsSpksMap.map {
      case (k, v) =>
        s"$k -> $v"
    }.toArray
  }
}
