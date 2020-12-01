package com.mhm.livewallet.test

import com.mhm.connectors.BitcoinSConnector
import com.mhm.connectors.RpcWrap.wrap
import org.bitcoins.core.protocol.BitcoinAddress
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class ScripthashSubscribeTest extends FlatSpec with ServiceFixture {

  val subscribeResult = service.blockchainScripthashSubcribe("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")

  println(s"5be0 ==> ${service.currentMonitorState.get.addressHistory.m.contains("5be022609383d23e2d545b3b359446466c269686c1e697b60355424ed30490d2")}")

  subscribeResult.length shouldBe 64

  subscribeResult shouldBe "6a7d8a2f84342ae7e3cca103ec43672b7185895e84e5d3ec8a17a5bc98692ada"

  /**
   * note - prefix "19" in the below is a problem - we need to remove it in a place where we use validateAddress - deterministic wallet
   * see note there, DeterministicWallet.getAddresses, ca. line 30
   */
  private val validationResult = BitcoinSConnector.rpcCli.validateAddress(BitcoinAddress("12tohASdGUCDFvqaygaGbL7Jub7CiHdwa4"))
  wrap(validationResult).scriptPubKey.map(_.hex).getOrElse(fail) shouldBe "1976a91414c45115d49cf4568d0d7229a9a0c58b64041c5388ac"
}
