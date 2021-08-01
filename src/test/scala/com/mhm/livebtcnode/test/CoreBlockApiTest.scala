/**
 * Copyright (c) 2020-2021 epsmi developers (see AUTHORS)
 *
 * This file is part of binglib.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package com.mhm.livebtcnode.test

import com.mhm.api4electrum.Api4ElectrumCore
import com.mhm.connectors.RpcWrap
import org.scalatest.FlatSpec
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.rpcCli
import com.mhm.epsmi.testbtcrpc.TestBitcoinSConnector.ec
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.scalatest.Matchers.convertToAnyShouldWrapper

class CoreBlockApiTest extends FlatSpec {

  val input = Seq(
    "00000000000000000005761a39f20efe35068837724bb4c0bc2875b66dab399b",
    "00000000000000000003882188eecf41bbdc86e6112a92a85623985b0249669c",
    "0000000000000000000045a5c310d8f8fbdb51557c80e59b96654b4f338c12c7",
    "000000000000000000066fdbc377cbee7b693467f96526fcabdbc935ec43620b"
  )

  val expectedHeaderHex = Seq(
    "00000020c40f4f52c3f1b6ebeb6204affb989a4ade8fe8c07ffe0c000000000000000000beb680afb1ba2ba8cf322211a7be9b2043f5819b187a3cb66de76488e22b72bcadc37a5fde950e173c7c0a6b",
    "000040203a2e538aae31df2b881c74882474427a6a0f305709db07000000000000000000bbe2e056a0b6f75be455e7213d1885e43cca00a451b61bd10c9d21f92be21670aa5b7b5fde950e1794f29dcb",
    "000000209c6649025b982356a8922a11e686dcbb41cfee882188030000000000000000001e16a83430e2b4d7a159a745c10f6e668db89a70ddec8bb8504930e8e2afb2fd4d5d7b5fde950e17c86d8282",
    "00008020c7128c334f4b65969be5807c5551dbfbf8d810c3a545000000000000000000002324c31d6a96c530d76208dd458adf4ae38a524f105b9ea9b18a7f960a60c3ab755e7b5fde950e175566d315"
  )

  val expectedNextBlockhash = Seq(
    "00000000000000000006ef710c2529be0c0afeca165253438376b811feff00d0",
    "0000000000000000000045a5c310d8f8fbdb51557c80e59b96654b4f338c12c7",
    "000000000000000000066fdbc377cbee7b693467f96526fcabdbc935ec43620b",
    "00000000000000000007d85bb3583e26dfd9c45882e0005a0899544d85b5525a"
  )
  "getBlockHeaderHashFromBlockHash" should "return header hex and optional next blockhash" in {
    val core = Api4ElectrumCore(rpcCli)
    input.indices.foreach { i =>
      val (headerHex, nextBlockhash) =
        RpcWrap.wrap(core.getBlockHeaderHashFromBlockHash(DoubleSha256DigestBE.fromHex(input(i))))
      headerHex shouldBe expectedHeaderHex(i)
      nextBlockhash.map(_.hex).getOrElse(fail) shouldBe expectedNextBlockhash(i)
    }
  }
}
