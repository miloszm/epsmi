package com.mhm.epsmi.protocol.unittest

import com.mhm.api4electrum.Api4ElectrumCore
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.dummyprotocol.DummyBtcProtocolRpc
import org.scalatest.FlatSpec

class DummyBtcProtocolRpcSpec extends FlatSpec {

//  "DummyBtcProtocolRpc" should "support GetBlockHeader" in {
//    val rpc = DummyBtcProtocolRpc()
//    for (height <- 0 until 1000)
//      for (raw <- Seq(true, false)){
//        val blockhash = wrap(rpc.getBlockHash(height))
//          Api4ElectrumCore(rpc).getBlockHeader(blockhash, raw)
//           TODO 1) refactor Api4ElectrumCore so that it can accept rpc, currently it is hardcoded
//           TODO 2) getBlockHeader should also accept "raw" argument, currently this is missing
//      }
//  }

}
