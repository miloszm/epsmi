package com.mhm.bitcoin.integration.test

import com.mhm.bitcoin.{AddressHistory, HistoryEntry, TransactionMonitor}
import com.mhm.util.HashOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class TransactionMonitorTest extends FlatSpec with IntTestFixture {

  "transaction monitor" should "gather history of transactions" in {
    val monitoredScriptPubKeys = Seq(
      "76a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac",
      "76a9143fe7f4ee744d330cbcc8ec5d68925e63ce03f77888ac",
      "76a914a2946db89edc09f56960cee76dab97604f7ffef088ac"
    )
    val addressHistory = TransactionMonitor.buildAddressHistory(monitoredScriptPubKeys)
    monitoredScriptPubKeys.foreach { k =>
      addressHistory.m(HashOps.script2ScriptHash(k)) shouldBe HistoryEntry(subscribed = false, Nil)
    }
    print(addressHistory.m)
  }

}
