package com.mhm.bitcoin

trait TransactionMonitor {

  def buildAddressHistory(monitoredScriptPubKeys: Seq[String])

}
