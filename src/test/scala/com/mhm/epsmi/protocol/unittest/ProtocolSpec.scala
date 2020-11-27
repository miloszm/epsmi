package com.mhm.epsmi.protocol.unittest

import java.io.ByteArrayOutputStream

import com.mhm.api4electrum.{Api4ElectrumCore, Api4ElectrumImpl}
import com.mhm.bitcoin.{TransactionMonitorFactory, TransactionMonitorState}
import com.mhm.epsmi.dummymonitor.DummyBtcRpc
import org.scalatest.FlatSpec

class ProtocolSpec extends FlatSpec {

  val dummyTransactionMonitor = TransactionMonitorFactory.create(DummyBtcRpc(Nil, Nil, Map()))

  val monitorState = TransactionMonitorState.createEmpty()

  val protocol = new Api4ElectrumImpl(Api4ElectrumCore(DummyBtcRpc(Nil, Nil, Map())), dummyTransactionMonitor, monitorState)

  val streamOutput = new ByteArrayOutputStream()

  protocol.triggerHeartbeatConnected(streamOutput)

  println(s"output=${streamOutput.toString}")

}
