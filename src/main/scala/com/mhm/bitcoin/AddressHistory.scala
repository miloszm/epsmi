package com.mhm.bitcoin

import com.mhm.common.model.HashHeight

case class HistoryElement (
  txHash: String,
  height: Int,
  fee: BigDecimal = 0
){
  def asHashHeight = HashHeight(txHash, height)
}

case class HistoryEntry(
  subscribed: Boolean,
  history: Seq[HistoryElement]
)

case class AddressHistory(m: Map[String, HistoryEntry])
