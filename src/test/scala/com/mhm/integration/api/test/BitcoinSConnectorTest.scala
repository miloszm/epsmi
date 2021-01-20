package com.mhm.integration.api.test

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.TransactionMonitorState
import com.mhm.epsmi.dummyprotocol.DummyTransactionMonitor
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

import scala.concurrent.ExecutionContext.Implicits.global

object BitcoinSConnectorTest extends FlatSpec {

  val blockHeader =
    new Api4ElectrumImpl(
      Api4ElectrumCore(TestBitcoinSConnector.rpcCli),
      new DummyTransactionMonitor,
      TransactionMonitorState.createEmpty()
    ).blockchainBlockHeader(652221)

  blockHeader shouldBe "00004020e028cc7a4447b6edb4a70c9165b7e11e2908c50483aa000000000000000000005df5cc1d04d234630bd7db39936d231a5f1ebb877d53be3961d88d37ed1be12340c9825fde950e172ea7194d"

}
