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

import com.mhm.main.Constants.BTC_RPC_TIMEOUT
import grizzled.slf4j.Logging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Awaitable}

object RpcWrap extends Logging {

  def wrap[T](awaitable: Awaitable[T], callDescription: String = ""): T = {
    if (!callDescription.isEmpty) {
      if (!List("listTransactions").contains(callDescription))
        logger.info(s"btcrpc: $callDescription")
    }
    try {
      Await.result(awaitable, BTC_RPC_TIMEOUT.seconds)
    } catch {
      case e: Throwable =>
        logger.error(s"caught: ${e.getClass.getCanonicalName} - ${e.getMessage} - $callDescription")
        throw e
    }
  }

}
