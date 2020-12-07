package com.mhm.epsmi.testbtcrpc

import com.mhm.connectors.BitcoinSConnector

import scala.concurrent.ExecutionContext

object TestBitcoinSConnector {

  val bitcoinSConnector = BitcoinSConnector(false, "foo", "bar")

  val bitcoindInstance = bitcoinSConnector.bitcoindInstance

  val system = bitcoinSConnector.system

  val rpcCli = bitcoinSConnector.rpcCli

  implicit val ec = ExecutionContext.global

}
