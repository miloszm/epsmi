package com.mhm.bitcoin


case class ReorganizableTxEntry(txid: String, blockhash: String, height: Int, matchingShs: Seq[String])
case class UnconfirmedTxEntry(txid: String, matchingShs: Seq[String])
case class TxidAddress(txid: String, address: String)


case class TransactionMonitorState(
  addressHistory: AddressHistory,
  unconfirmedTxes: Map[String,Seq[String]] = Map(),
  reorganizableTxes: Seq[ReorganizableTxEntry] = Seq(),
  updatedScripthashes: Seq[String] = Seq(),
  lastKnownTx: Option[TxidAddress] = None
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
  def combineUpdatedScripthashes(other: TransactionMonitorState): TransactionMonitorState = {
    this.copy(updatedScripthashes = this.updatedScripthashes ++ other.updatedScripthashes)
  }
}
