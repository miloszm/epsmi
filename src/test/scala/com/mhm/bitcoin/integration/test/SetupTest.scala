package com.mhm.bitcoin.integration.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.setup.Setup
import com.typesafe.config.ConfigFactory
import org.scalatest.FlatSpec

import scala.concurrent.Await
import scala.concurrent.duration.DurationInt


class SetupTest extends FlatSpec {

  "setup" should "obtain list of script pub keys to monitor" in {
    val config = ConfigFactory.load()
    val resultFut = Setup.getScriptPubKeysToMonitor(BitcoinSConnector.rpcCliExt, config)
    val result = Await.result(resultFut, 20.seconds)
    println(s"setup result=$result")
  }

}
