package com.mhm.connectors

import grizzled.slf4j.Logging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Awaitable}

object RpcWrap extends Logging {

  def wrap[T](awaitable: Awaitable[T], callDescription: String = ""): T = {
    if (!callDescription.isEmpty)
      logger.trace(s"btcrpc: $callDescription")
    try {
      val r: Awaitable[T] = awaitable
      Await.result(awaitable, 40.seconds)
    }
    catch {
      case e: Throwable =>
        logger.error(s"caught: ${e.getClass.getCanonicalName} - ${e.getMessage}")
        throw e
    }
  }

}
