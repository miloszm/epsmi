package com.mhm.bitcoin

object AddressHistory {

  case class HistoryElement (
    txHash: String,
    height: Int,
    fee: Int
  )

  case class HistoryEntry(
    subscribed: Boolean,
    history: Seq[HistoryElement]
  )

  val m = Map[String, HistoryEntry]()

}
