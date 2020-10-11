package com.mhm.api4electrum

import com.mhm.api4electrum.Api4ElectrumCore.getBlockHeaderHash

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, SECONDS}

/**
 * This class conforms to json rpc requirements.
 * Uses Api4ElectrumCore for everything else.
 */

class Api4ElectrumImpl extends Api4Electrum {
  override def serverVersion(v1: String, v2: String): Array[String] = {
    return Array("epsmi 0.0.2")
  }
  override def blockchainBlockHeader(height: Int): String = {
    return Await.result(getBlockHeaderHash(height), Duration(20, SECONDS))
  }
}
