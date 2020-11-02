package com.mhm.bitcoin.test

import com.mhm.wallet.WalletUtil
import com.mhm.wallet.WalletUtil.KeyRawTuple
import org.scalatest.FlatSpec
import org.scalatest.Matchers.convertToAnyShouldWrapper
import scodec.bits._

class WalletUtilTest extends FlatSpec {

  "convertToStandardXpub" should "return standard xpub" in {
    val mpk = "tpubD6NzVbkrYhZ4YVMVzC7wZeRfz3bhqcHvV8M3UiULCfzFtLtp5nwvi6LnBQegrkxYGPkSzXUEvcPEHcKdda8W1YShVBkhFBGkLxjSQ1Nx3cJ"
    val xpub = WalletUtil.convertToStandardXpub(mpk)
    //xpub shouldBe "xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ"
    println(xpub)
  }

  "bip32serialize for extended key" should "return serialized extended key given elements as in BIP32 serialization format" in {
    val keyRawTuple = KeyRawTuple(
      hex"0488b21e",
      0,
      hex"00000000",
      0,
      hex"d801acec5ee718de5c99b50791e2febecf2490733ebbf16128ab6b56b48303ec",
      hex"026fca11cede24f656a8dd74564a9e7fd5307378688ae41c8912dab6562761fb37"
    )
    val serKey = WalletUtil.bip32Serialize(keyRawTuple)
    serKey shouldBe "xpub661MyMwAqRbcGhAeY18rMj6JevW3rDnrwVkk8ARBrmEN94E71a6qZefwwNZuLG1JUVDYNwxNPWZBbRq2EiHG86zQRb1PZecWm192dyCt7SJ"
  }

}
