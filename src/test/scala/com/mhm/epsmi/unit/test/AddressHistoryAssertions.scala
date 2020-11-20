package com.mhm.epsmi.unit.test

import com.mhm.bitcoin.AddressHistory
import com.mhm.util.HashOps
import org.scalatest.Matchers

trait AddressHistoryAssertions extends Matchers {

  def assertAddressHistoryTx(addressHistory: AddressHistory, spk: String, height: Int, txId: String, subscribed: Boolean): Unit = {
    val historyElement = addressHistory.m.getOrElse(HashOps.script2ScriptHash(spk), fail)
    historyElement.history.head.height shouldBe height
    historyElement.history.head.txHash shouldBe txId
    if (height == 0)
      historyElement.history.head.fee shouldBe 0
    historyElement.subscribed shouldBe subscribed
  }

}
