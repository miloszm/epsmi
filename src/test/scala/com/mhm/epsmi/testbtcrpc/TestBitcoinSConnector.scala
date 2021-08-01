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
