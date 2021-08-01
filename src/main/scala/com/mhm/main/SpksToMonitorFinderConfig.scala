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
package com.mhm.main

import java.util

import com.typesafe.config.{Config, ConfigValue}
import scala.jdk.CollectionConverters.SetHasAsScala

/**
  * mpk stands for master public key
  * woa stands for watch-only addresses
  */
case class SpksToMonitorFinderConfig(isTestnet: Boolean,
                                     mpks: Seq[util.Map.Entry[String, ConfigValue]],
                                     woas: Seq[util.Map.Entry[String, ConfigValue]],
                                     initialImportCount: Int)

object SpksToMonitorFinderConfig {

  def init(config: Config): SpksToMonitorFinderConfig = {
    val isTestnet = config.getBoolean("epsmi.testnet")
    val mpkConfig =
      config.getConfig(s"epsmi.master-public-keys${if (isTestnet) "-testnet" else ""}")
    val mpks = mpkConfig.entrySet().asScala.toSeq
    val woaConfig =
      config.getConfig(s"epsmi.watch-only-addresses${if (isTestnet) "-testnet" else ""}")
    val woas               = woaConfig.entrySet().asScala.toSeq
    val initialImportCount = config.getInt("epsmi.initial-import-count")
    SpksToMonitorFinderConfig(isTestnet, mpks, woas, initialImportCount)
  }
}
