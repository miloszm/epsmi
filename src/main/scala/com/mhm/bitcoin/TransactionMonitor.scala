package com.mhm.bitcoin

import com.mhm.util.HashOps

object TransactionMonitor {

  def buildAddressHistory(monitoredScriptPubKeys: Seq[String]): AddressHistory = {
    val ah = new AddressHistory(
      scala.collection.mutable.HashMap.from(monitoredScriptPubKeys.map(k => HashOps.script2ScriptHash(k) -> HistoryEntry(false, Nil)))
    )
    ah
  }

}
