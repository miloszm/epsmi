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

}
