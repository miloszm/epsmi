package com.mhm.bitcoin.test

import com.mhm.util.BaseOps
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits.HexStringSyntax

class BaseOpsTest extends FlatSpec {
  "decode" should "convert base58 encoded number into an integer" in {
    val decoded = BaseOps.decode("tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ", 58)
    decoded.toString shouldBe "4916168930508585312200884129123615957414675087408968371286219776133926492523251012231716720806962455048291126486067511577439222578945229342172280912299270752776086954255181781808200627932716490095"
  }

  "decode" should "convert 256-encoded string into an integer" in {
    val decoded = BaseOps.decode("d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec", base = 256)
    decoded.toString shouldBe "97702535613963802964819910544120887115295631296599048914567293081941526643692"
  }

  "decodeBytes" should "convert byte array into an integer" in {
    val decoded = BaseOps.decodeBytes(hex"d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec")
    decoded.toString shouldBe "97702535613963802964819910544120887115295631296599048914567293081941526643692"
  }

}
