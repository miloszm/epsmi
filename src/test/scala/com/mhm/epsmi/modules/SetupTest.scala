package com.mhm.epsmi.modules

import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper


class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val result = Setup.getScriptPubKeysToMonitor(BitcoinSConnector.rpcCli, config)
    result.spksToMonitor.isEmpty shouldBe false
    result.wallets.isEmpty shouldBe false
    println(s"setup result=$result")
  }

}
