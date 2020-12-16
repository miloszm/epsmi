package com.mhm.main

import java.util

import com.typesafe.config.{Config, ConfigValue}
import scala.jdk.CollectionConverters.SetHasAsScala

case class SpksToMonitorFinderConfig(
  isTestnet: Boolean,
  mpks: Seq[util.Map.Entry[String, ConfigValue]],
  initialImportCount: Int
)

object SpksToMonitorFinderConfig {
  def init(config: Config): SpksToMonitorFinderConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val mpkConfig = config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
    val mpks = mpkConfig.entrySet().asScala.toSeq
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    SpksToMonitorFinderConfig(isTestnet, mpks, initialImportCount)
  }
}
