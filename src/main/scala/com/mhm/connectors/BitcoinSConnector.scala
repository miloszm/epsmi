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
package com.mhm.connectors

import java.net.URI

import akka.actor.ActorSystem

case class BitcoinSConnector(isTestnet: Boolean,
                             username: String, //this username comes from 'rpcuser' in your bitcoin.conf file
                             password: String //this password comes from your 'rpcpassword' in your bitcoin.conf file
) {
  import org.bitcoins.core.config._
  import org.bitcoins.rpc.config._

  val authCredentials =
    BitcoindAuthCredentials.PasswordBased(username = username, password = password)

  lazy val bitcoindInstance = if (isTestnet) {
    BitcoindInstance(
      network         = TestNet3,
      uri             = new URI(s"""http://localhost:${TestNet3.port}"""),
      rpcUri          = new URI(s"""http://localhost:${TestNet3.rpcPort}"""),
      authCredentials = authCredentials
    )
  } else {
    BitcoindInstance(
      network         = MainNet,
      uri             = new URI(s"http://localhost:${MainNet.port}"),
      rpcUri          = new URI(s"http://localhost:${MainNet.rpcPort}"),
      authCredentials = authCredentials
    )
  }

  /**
    * we have to use internal bitcoin-s actor system name here
    * as bitcoin-s it does not cover all APIs
    * since we have 2 clients, one "normal" and one "extended"
    * we have to have the actor system here, hence we need to
    * use the name here
    */
  val actorSystemName = "bitcoind-rpc-client-created-by-bitcoin-s"

  implicit val system = ActorSystem.create(actorSystemName)

  //val rpcCli = BitcoindRpcClient.withActorSystem(bitcoindInstance)

  /**
    * extended rpc client with more APIs - due to the strangeness of bitcoin-s not covering
    * newer APIs easily, for example, getaddressesbylabel
    */
  lazy val rpcCli = new BitcoindRpcExtendedClient(bitcoindInstance, implicitly[ActorSystem])

}
