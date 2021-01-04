package com.mhm.integration.api.test

import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper

class BlockApiTest extends FlatSpec with IntTestFixture {

  "blockchain.block.headers" should "return block headers" in {
    val headersResult = fixture.client.blockchainBlockHeaders(652510, 512)
    val source = scala.io.Source.fromInputStream(this.getClass.getResourceAsStream("/blocktest/blockheaders.txt"))
    val expectedResult = try source.mkString finally source.close()
    headersResult.hex shouldBe expectedResult
  }

  "blockchain.block.header" should "return block header" in {
     val hex = fixture.client.blockchainBlockHeader(652221)
     hex shouldBe "00004020e028cc7a4447b6edb4a70c9165b7e11e2908c50483aa000000000000000000005df5cc1d04d234630bd7db39936d231a5f1ebb877d53be3961d88d37ed1be12340c9825fde950e172ea7194d"
  }

  "blockchain.block.get_header" should "return block header information" in {
    val headerResult = fixture.client.blockchainBlockGetHeader(652221)
    headerResult.block_height shouldBe 652221
    headerResult.prev_block_hash shouldBe "00000000000000000000aa8304c508291ee1b765910ca7b4edb647447acc28e0"
    headerResult.merkle_root shouldBe "23e11bed378dd86139be537d87bb1e5f1a236d9339dbd70b6334d2041dccf55d"
    headerResult.version shouldBe 541065216
    headerResult.nonce shouldBe 1293526830
    headerResult.bits shouldBe 386831838
  }

  "blockchain.block.get_chunk" should "return correct block header information" in {
    val chunk = fixture.client.blockchainBlockGetChunk(323)
    chunk.startsWith("00004020544d495e9eb2a8bd0bf278d68b51824") shouldBe true
    chunk.endsWith("762765dbb889efa200f8b5fde950e17b347671f") shouldBe true
    chunk.length shouldBe 322560
  }
}
