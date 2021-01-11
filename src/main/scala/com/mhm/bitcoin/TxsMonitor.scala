package com.mhm.bitcoin

import java.util.concurrent.atomic.{AtomicLong, AtomicReference}


trait TxsMonitorStateListener {
  def updated(newState: TransactionMonitorState): Unit
  def heartbeatTick(): Unit
  def updatedShsTick(n: Int): Unit
}

object NoopTxsMonitorStateListener extends TxsMonitorStateListener {
  override def updated(newState: TransactionMonitorState): Unit = ()
  override def heartbeatTick(): Unit = ()
  override def updatedShsTick(n: Int): Unit = ()
}

trait TxsMonitorMBean {
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

  // set methods to allow copy/paste in jconsole only
  def setAddressHistory(a: Array[String]) = ()
  def setNonEmptyAddressHistory(a: Array[String]) = ()
  def setUnconfirmedTxs(a: Array[String]) = ()
  def setReorganizableTxs(a: Array[String]) = ()
  def setUpdatedShs(a: Array[String]) = ()
  def setLastKnownTx(s: String) = ()
  def setLastKnownAddress(s: String) = ()
  def setSubscribedToHeaders(b: Boolean) = ()
}

class TxsMonitor extends TxsMonitorMBean with TxsMonitorStateListener {
  val currentState = new AtomicReference[TransactionMonitorState]()
  val heartbeatCounter = new AtomicLong()
  val updatedShsCounter = new AtomicLong()
  override def getAddressHistory: Array[String] = {
    currentState.get.addressHistory.m.collect{
    case (sh, historyEntry) =>
      s"sh=$sh -> ${historyEntry.history.map(he => s"txid=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getNonEmptyAddressHistory: Array[String] = {
    currentState.get.addressHistory.m.collect{
    case (sh, historyEntry) if historyEntry.history.nonEmpty =>
      s"sh=$sh -> ${historyEntry.history.map(he => s"txid=${he.txHash} height=${he.height}").mkString("|")}"
    }.toArray
  }
  override def getUnconfirmedTxs: Array[String] = {
    currentState.get.unconfirmedTxes.map{ case (sh, txids) =>
      s"txid=$sh -> shs=${txids.mkString("|")}"
    }.toArray
  }
  override def getReorganizableTxs: Array[String] = {
    currentState.get.reorganizableTxes.map{ entry =>
      s"txid=${entry.txid} height=${entry.height} blockhash=${entry.blockhashOpt.getOrElse("n/a")} shs=${entry.matchingShs.mkString("|")}"
    }.toArray
  }
  override def getUpdatedShs: Array[String] = {
    currentState.get.updatedScripthashes.toArray
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
  override def heartbeatTick(): Unit = {
    heartbeatCounter.incrementAndGet()
  }
  override def getHeartbeatTick: Long = {
    heartbeatCounter.get()
  }
  override def updatedShsTick(n: Int): Unit = {
    updatedShsCounter.addAndGet(n)
  }

  override def getUpdatedShsTick: Long = {
    updatedShsCounter.get()
  }

}
