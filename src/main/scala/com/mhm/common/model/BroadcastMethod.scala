package com.mhm.common.model

sealed trait BroadcastMethod

object OwnNode extends  BroadcastMethod
object UnsupportedBroadcastMethod extends BroadcastMethod
