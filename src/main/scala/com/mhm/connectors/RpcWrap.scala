package com.mhm.connectors

import grizzled.slf4j.Logging

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Awaitable}

object RpcWrap extends Logging {

  def wrap[T](awaitable: Awaitable[T], callDescription: String = ""): T = {
    val r = Await.result(awaitable, 20.seconds)
    if (!callDescription.isEmpty)
      logger.debug(s"btcrpc: $callDescription")
    r
  }

}
