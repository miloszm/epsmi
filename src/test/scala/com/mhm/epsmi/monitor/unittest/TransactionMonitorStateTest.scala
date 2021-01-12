package com.mhm.epsmi.monitor.unittest

import com.mhm.bitcoin.TransactionMonitorState
import com.mhm.common.model.{AddressHistory, HistoryElement, HistoryEntry}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{contain, convertToAnyShouldWrapper}

class TransactionMonitorStateTest extends FlatSpec {

  "init unconfirmed txs" should "collect unconfirmed txs and relevant shs from address history" in {
    val addressHistory = AddressHistory(Map(
      "sh1" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx1", 665730))),
      "sh2" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx2", 0))),
      "sh3" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx3", -1))),
      "sh4" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx4", 665731), HistoryElement("tx3", 0))),
      "sh5" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx4", 665731), HistoryElement("tx5", 0), HistoryElement("tx6", 0)))
    ))
    val state = TransactionMonitorState(addressHistory).initUnconfirmedTxs()
    state.unconfirmedTxes.size shouldBe 4
    state.unconfirmedTxes.keys should contain theSameElementsAs Seq("tx2", "tx3", "tx5", "tx6")
    state.unconfirmedTxes("tx2") should contain theSameElementsAs Seq("sh2")
    state.unconfirmedTxes("tx3") should contain theSameElementsAs Seq("sh3", "sh4")
    state.unconfirmedTxes("tx5") should contain theSameElementsAs Seq("sh5")
    state.unconfirmedTxes("tx6") should contain theSameElementsAs Seq("sh5")
  }

  "sort address history" should "sort according to height, putting unconfirmed last" in {
    val addressHistory = AddressHistory(Map(
      "sh1" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx3", -1), HistoryElement("tx3", 0), HistoryElement("tx3", 500000), HistoryElement("tx3", 600000))),
      "sh2" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx3", 0), HistoryElement("tx3", 20), HistoryElement("tx3", 50), HistoryElement("tx3", 10))),
      "sh3" -> HistoryEntry(subscribed = true, Seq(HistoryElement("tx3", 1000), HistoryElement("tx3", 999), HistoryElement("tx3", 998), HistoryElement("tx3", 900))),
    ))
    val state = TransactionMonitorState(addressHistory).sortAddressHistory()
    def getHeights(sh: String) = state.addressHistory.m.getOrElse(sh, fail).history.map(_.height)
    getHeights("sh1") should contain theSameElementsInOrderAs Seq(500000, 600000, -1, 0)
    getHeights("sh2") should contain theSameElementsInOrderAs Seq(10, 20, 50, 0)
    getHeights("sh3") should contain theSameElementsInOrderAs Seq(900, 998, 999, 1000)
  }

}
