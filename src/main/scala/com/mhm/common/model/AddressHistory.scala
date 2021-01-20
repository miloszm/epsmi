package com.mhm.common.model

case class HistoryElement(txHash: String, height: Int, fee: BigDecimal = 0) {
  def asHashHeight = HexHeight(txHash, height)
}

case class HistoryEntry(subscribed: Boolean, history: Seq[HistoryElement])

case class AddressHistory(m: Map[String, HistoryEntry])
