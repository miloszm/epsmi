package com.mhm.connectors

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Awaitable}

object RpcWrap {

  def wrap[T](awaitable: Awaitable[T], callDescription: String): T = {
    val r = Await.result(awaitable, 20.seconds)
    println(s"btcrpc: $callDescription")
    r
  }

}
