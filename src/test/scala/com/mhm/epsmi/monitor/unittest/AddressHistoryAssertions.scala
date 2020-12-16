package com.mhm.epsmi.monitor.unittest

import com.mhm.common.model.AddressHistory
import com.mhm.util.HashOps
import org.scalatest.Matchers

trait AddressHistoryAssertions extends Matchers {

  def assertAddressHistoryTx(addressHistory: AddressHistory, spk: String, height: Int, txId: String, subscribed: Boolean): Unit = {
    val historyEntry = addressHistory.m.getOrElse(HashOps.script2ScriptHash(spk), fail)
    historyEntry.history.head.height shouldBe height
    historyEntry.history.head.txHash shouldBe txId
//    if (height == 0)
//      historyEntry.history.head.fee shouldBe 0 // TODO
    historyEntry.subscribed shouldBe subscribed
  }

}
