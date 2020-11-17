package com.mhm.bitcoin


case class ReorganizableTxEntry(txid: String, blockhash: String, height: Int, matchingShs: Seq[String])
case class UnconfirmedTxEntry(txid: String, matchingShs: Seq[String])

case class TransactionMonitorState(
  addressHistory: AddressHistory,
  unconfirmedTxes: Map[String,Seq[String]] = Map(),
  reorganizableTxes: Seq[ReorganizableTxEntry] = Seq(),
  updatedScripthashes: Seq[String] = Seq()
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
      case (k, v) if shs.contains(k) => k -> v.copy(history = v.history.filter(_.txHash == txid))
      case (k, v) => k -> v
    }
    this.copy(addressHistory = this.addressHistory.copy(m = newMap))
  }
  def addHistoryItemForScripthashes(shs: Seq[String], he: HistoryElement): TransactionMonitorState = {
    val newMap = addressHistory.m.collect {
      case (k, v) if shs.contains(k) => k -> v.copy(history = v.history :+ he)
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
    val newUnconfirmedTxes = unconfirmedTxes.collect {
      case (k, v) if k == txid => k -> (v ++ shs)
      case (k, v) => k -> v
    }
    this.copy(unconfirmedTxes = newUnconfirmedTxes)
  }
}

