package com.mhm.livebtcnode.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import com.mhm.main.SpksToMonitorFinder
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val result = new SpksToMonitorFinder(TestBitcoinSConnector.rpcCli, config).getScriptPubKeysToMonitor()
    result.spksToMonitor.nonEmpty shouldBe true
    result.wallets.nonEmpty shouldBe true
    println(s"getScriptPubKeysToMonitor result:")
    println(s"${result.spksToMonitor.size} spks to monitor, head is ${result.spksToMonitor.head}")
    println(s"wallets=${result.wallets}")
  }

}
