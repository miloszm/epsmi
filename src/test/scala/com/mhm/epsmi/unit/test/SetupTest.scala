package com.mhm.epsmi.unit.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper


class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val result = new Setup(BitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
    result.spksToMonitor.isEmpty shouldBe false
    result.wallets.isEmpty shouldBe false
    println(s"getScriptPubKeysToMonitor result:")
    println(s"${result.spksToMonitor.size} spks to monitor, head is ${result.spksToMonitor.head}")
    println(s"wallets=${result.wallets}")
  }

}
