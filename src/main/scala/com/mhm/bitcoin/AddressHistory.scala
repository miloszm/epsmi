package com.mhm.bitcoin

case class HistoryElement (
  txHash: String,
  height: Int,
  fee: BigDecimal = 0
)

case class HistoryEntry(
  subscribed: Boolean,
  history: Seq[HistoryElement]
)

class AddressHistory(val m: scala.collection.mutable.Map[String, HistoryEntry]) {
}
