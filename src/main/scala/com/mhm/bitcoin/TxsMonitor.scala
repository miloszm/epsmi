package com.mhm.bitcoin

import java.util.concurrent.atomic.AtomicReference


trait TxsMonitorStateListener {
  def updated(newState: TransactionMonitorState)
}

object NoopTxsMonitorStateListener extends TxsMonitorStateListener {
  override def updated(newState: TransactionMonitorState): Unit = ()
}

trait TxsMonitorMBean {
  def getAddressHistory: Array[String]
  def getNonEmptyAddressHistory: Array[String]
  def getUnconfirmedTxs: Array[String]
  def getReorganizableTxs: Array[String]
  def getLastKnownTx: String
  def getLastKnownAddress: String
  def getSubscribedToHeaders: Boolean
}

class TxsMonitor extends TxsMonitorMBean with TxsMonitorStateListener {
  val currentState = new AtomicReference[TransactionMonitorState]()
  override def getAddressHistory: Array[String] = {
    currentState.get.addressHistory.m.collect{
    case (sh, historyEntry) =>
      s"sh=$sh -> ${historyEntry.history.map(he => s"tx=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getNonEmptyAddressHistory: Array[String] = {
    currentState.get.addressHistory.m.collect{
    case (sh, historyEntry) if historyEntry.history.nonEmpty =>
      s"sh=$sh -> ${historyEntry.history.map(he => s"tx=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getUnconfirmedTxs: Array[String] = {
    currentState.get.unconfirmedTxes.map{ case (sh, txids) =>
      s"sh=$sh -> ${txids.mkString("|")}"
    }.toArray
  }
  override def getReorganizableTxs: Array[String] = {
    currentState.get.reorganizableTxes.map{ entry =>
      s"txid=${entry.txid} height=${entry.height} blockhash=${entry.blockhashOpt.getOrElse("n/a")} relevant shs=${entry.matchingShs.mkString("|")}"
    }.toArray
  }
  override def getLastKnownTx: String = {
    currentState.get.lastKnownTx.map{ _.txid }.getOrElse("n/a")
  }
  override def getLastKnownAddress: String = {
    currentState.get.lastKnownTx.map{ _.address }.getOrElse("n/a")
  }
  override def getSubscribedToHeaders: Boolean = {
    currentState.get().subscribedToHeaders
  }
  override def updated(newState: TransactionMonitorState): Unit = {
    currentState.set(newState)
  }
}
