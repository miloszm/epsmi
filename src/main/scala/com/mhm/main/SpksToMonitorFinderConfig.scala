package com.mhm.main

import java.util

import com.typesafe.config.{Config, ConfigValue}
import scala.jdk.CollectionConverters.SetHasAsScala

/**
 * mpk stands for master public key
 * woa stands for watch-only addresses
 */

case class SpksToMonitorFinderConfig(
  isTestnet: Boolean,
  mpks: Seq[util.Map.Entry[String, ConfigValue]],
  woas: Seq[util.Map.Entry[String, ConfigValue]],
  initialImportCount: Int
)

object SpksToMonitorFinderConfig {
  def init(config: Config): SpksToMonitorFinderConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val mpkConfig = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
    val mpks = mpkConfig.entrySet().asScala.toSeq
    val woaConfig = config.getConfig(s"epsmi.watch-only-addresses${if (isTestnet) "-testnet" else ""}")
    val woas = woaConfig.entrySet().asScala.toSeq
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    SpksToMonitorFinderConfig(isTestnet, mpks, woas, initialImportCount)
  }
}
