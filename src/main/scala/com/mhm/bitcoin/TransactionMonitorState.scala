package com.mhm.bitcoin

import com.mhm.util.HashOps.getStatusElectrum


case class ReorganizableTxEntry(txid: String, blockhash: String, height: Int, matchingShs: Seq[String])
case class UnconfirmedTxEntry(txid: String, matchingShs: Seq[String])
case class TxidAddress(txid: String, address: String)


object TransactionMonitorState {
  def createEmpty() : TransactionMonitorState = {
    TransactionMonitorState(AddressHistory(Map()))
  }
}


case class TransactionMonitorState(
  addressHistory: AddressHistory,
  unconfirmedTxes: Map[String,Seq[String]] = Map(),
  reorganizableTxes: Seq[ReorganizableTxEntry] = Seq(),
  updatedScripthashes: Seq[String] = Seq(),
  lastKnownTx: Option[TxidAddress] = None,
  subscribedToHeaders: Boolean = false
){
  def removeUnconfirmed(removed: Seq[UnconfirmedTxEntry]): TransactionMonitorState = {
    val newUnconfirmedTxes = unconfirmedTxes.collect {case e@(txid, _) if !removed.map(_.txid).contains(txid) => e }
    this.copy(unconfirmedTxes = newUnconfirmedTxes)
  }
  def removeReorganizable(removed: ReorganizableTxEntry): TransactionMonitorState = {
    this.copy(reorganizableTxes = this.reorganizableTxes.filterNot(r => r.txid == removed.txid && r.height == removed.height))
  }
  def deleteHistoryItemForScripthashes(shs: Seq[String], txid: String): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) => k -> v.copy(history = v.history.filterNot(_.txHash == txid))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def deleteHistoryItems(shs: Seq[String], txid: String, height: Int): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) => k -> v.copy(history = v.history.filterNot(e => e.txHash == txid && e.height == height))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def addHistoryItemForScripthashes(shs: Seq[String], he: HistoryElement): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) => k -> v.copy(history = he +: v.history.filterNot(a => a.txHash == he.txHash && a.height == he.height))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def updateHeightForScripthashes(shs: Seq[String], txid: String, newHeight: Int): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, h) if shs.contains(k) => k -> h.copy(history = h.history.map(e =>
        if (e.txHash == txid) e.copy(height = newHeight) else e )
      )
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def addReorganizableTx(reorganizableTxEntry: ReorganizableTxEntry): TransactionMonitorState = {
    this.copy(reorganizableTxes = this.reorganizableTxes :+ reorganizableTxEntry)
  }
  def addUpdatedScripthashes(shs: Seq[String]): TransactionMonitorState = {
    this.copy(updatedScripthashes = this.updatedScripthashes ++ shs)
  }
  def addUnconfirmedScripthases(txid: String, shs: Seq[String]): TransactionMonitorState = {
    val newValue = unconfirmedTxes.getOrElse(txid, Nil) ++ shs
    val newUnconfirmedTxes = (unconfirmedTxes - txid) + (txid -> newValue)
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
    val newMap = addressHistory.m.collect {
      case (k, v) => k -> sortAddressHistoryList(v)
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  private def sortAddressHistoryList(historyEntry: HistoryEntry): HistoryEntry = {
    val unconfirmedTxs = historyEntry.history.filter(_.height <= 0)
    val confirmedTxs = historyEntry.history.filter(_.height > 0)
    val sortedConfirmedTxs = confirmedTxs.sortWith((e1, e2) => e1.height < e2.height)
    historyEntry.copy(history = sortedConfirmedTxs ++ unconfirmedTxs)
  }
  def applyIf(b: Boolean)(f: TransactionMonitorState => TransactionMonitorState): TransactionMonitorState = {
    if (b) f(this) else this
  }
  def resetUpdatedScripthashes(): TransactionMonitorState = {
    this.copy(updatedScripthashes = Nil)
  }
  def getElectrumHistory(sh: String): Option[Seq[HistoryElement]] = {
    this.addressHistory.m.get(sh).map(_.history)
  }
  def subscribeAddress(sh: String): TransactionMonitorState = {
    val newMap = addressHistory.m.get(sh) match {
      case Some(he) => (addressHistory.m - sh) + (sh -> he.copy(subscribed = true))
      case None => addressHistory.m
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def initUnconfirmedTxes(): TransactionMonitorState = {
    val mutableMap = scala.collection.mutable.HashMap[String, Seq[String]]()
    addressHistory.m.foreach { case (sh, he) =>
      val unconfirmedTxids = he.history.filter(_.height <= 0).map(_.txHash)
      unconfirmedTxids.foreach { txid =>
        mutableMap.put(txid, mutableMap.getOrElse(txid, Nil) :+ sh)
      }
    }
    this.copy(unconfirmedTxes = mutableMap.toMap)
  }
  def getElectrumHistoryHash(sh: String): String = {
    val hashHeights = this.addressHistory.m.get(sh).map {
      _.history.map(_.asHashHeight)
    }.getOrElse(Nil)
    getStatusElectrum(hashHeights.toList)
  }

}
