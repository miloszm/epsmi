package com.mhm.bitcoin

sealed trait BroadcastMethod

object OwnNode extends  BroadcastMethod
object UnsupportedBroadcastMethod extends BroadcastMethod
