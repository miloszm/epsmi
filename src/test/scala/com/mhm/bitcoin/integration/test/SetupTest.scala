package com.mhm.bitcoin.integration.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt


class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val result = Setup.getScriptPubKeysToMonitor(BitcoinSConnector.rpcCli, BitcoinSConnector.rpcCliExt, config)
    result.spksToMonitor.isEmpty shouldBe false
    result.wallets.isEmpty shouldBe false
    println(s"setup result=$result")
  }

}
