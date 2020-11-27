package com.mhm.epsmi.protocol.unittest

import com.mhm.api4electrum.Api4ElectrumCore
import com.mhm.connectors.RpcWrap.wrap
import com.mhm.epsmi.dummyprotocol.DummyBtcProtocolRpc
import com.mhm.epsmi.dummyprotocol.DummyBtcProtocolRpc.DummyJsonrpcBlockchainHeight
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class DummyBtcProtocolRpcSpec extends FlatSpec {

  "DummyBtcProtocolRpc" should "support GetBlockHeader" in {
    val rpc = DummyBtcProtocolRpc()
    for (height <- 0 until 1000)
      for (raw <- Seq(true, false)){
        val blockhash = wrap(rpc.getBlockHash(height))
        val hashHeightOrHeader = wrap(Api4ElectrumCore(rpc).getBlockHeader(blockhash, raw))
          if (raw){
            hashHeightOrHeader.isRight shouldBe true
            hashHeightOrHeader.map(_.hash.length).getOrElse(fail) shouldBe 160
          } else {
            hashHeightOrHeader.isLeft shouldBe true
            hashHeightOrHeader.swap.map(_.version).getOrElse(fail) shouldBe 536870912
          }
      }
  }

  "DummyBtcProtocolRpc" should "support GetCurrentHeader" in {
    val rpc = DummyBtcProtocolRpc()
    for (raw <- Seq(true, false)){
      val r = wrap(Api4ElectrumCore(rpc).getCurrentHeader(raw))
      val (bestBlockHash, hashHeightOrHeader) = r
      if (raw){
        hashHeightOrHeader.isRight shouldBe true
        hashHeightOrHeader.map(_.hash.length).getOrElse(fail) shouldBe 160
      }
      else {
        hashHeightOrHeader.isLeft shouldBe true
        hashHeightOrHeader.swap.map(_.version).getOrElse(fail) shouldBe 536870912
      }
      bestBlockHash.nonEmpty shouldBe true
    }
  }

  "DummyBtcProtocolRpc" should "support GetBlockHeadersHex" in {
    val rpc = DummyBtcProtocolRpc()
    for (startHeight <- Seq(100, DummyJsonrpcBlockchainHeight + 10, DummyJsonrpcBlockchainHeight - 10, 0))
      for (count <- Seq(200, 5, 15, 250)){
        val result = wrap(Api4ElectrumCore(rpc).getBlockHeaders(startHeight, count))
        val availableBlocks = -Math.min(0, startHeight - DummyJsonrpcBlockchainHeight - 1)
        val expectedCount = Math.min(availableBlocks, count)
        result.hex.length shouldBe expectedCount*80*2 //#80 bytes/header, 2 chars/byte
        result.count shouldBe expectedCount
      }
  }


}
