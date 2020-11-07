package com.mhm.epsmi.base

import com.mhm.api4electrum.Api4ElectrumImpl
import com.mhm.connectors.BitcoinSConnector

import scala.sys.exit

object BitcoinSConnectorTest extends App {

  println(new Api4ElectrumImpl().blockchainBlockHeader(652221))
  println("00004020e028cc7a4447b6edb4a70c9165b7e11e2908c50483aa000000000000000000005df5cc1d04d234630bd7db39936d231a5f1ebb877d53be3961d88d37ed1be12340c9825fde950e172ea7194d".toUpperCase)

  exit(0)
}
