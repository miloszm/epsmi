package com.mhm.connectors

import com.mhm.main.Constants.BTC_RPC_TIMEOUT
import grizzled.slf4j.Logging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Awaitable}

object RpcWrap extends Logging {

  def wrap[T](awaitable: Awaitable[T], callDescription: String = ""): T = {
    if (!callDescription.isEmpty)
      logger.info(s"btcrpc: $callDescription")
    try {
      Await.result(awaitable, BTC_RPC_TIMEOUT.seconds)
    }
    catch {
      case e: Throwable =>
        logger.error(s"caught: ${e.getClass.getCanonicalName} - ${e.getMessage} - $callDescription")
        throw e
    }
  }

}
