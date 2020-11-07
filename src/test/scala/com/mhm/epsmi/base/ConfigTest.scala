package com.mhm.epsmi.base

import com.typesafe.config.{Config, ConfigFactory}
import org.scalatest.FlatSpec
import org.scalatest.Matchers.{convertToAnyShouldWrapper, have}

import scala.jdk.CollectionConverters.SetHasAsScala

class ConfigTest extends FlatSpec {

  "configuration code" should "be able to read master public keys" in {
    val config = ConfigFactory.load
    val mpkConfig = config.getConfig("epsmi.master-public-keys")
    val mpks = mpkConfig.entrySet()
    println(s"size of mpks=${mpks.size()}")
    mpks.asScala.foreach{ e =>
      println(s"key=${e.getKey} value=${e.getValue.unwrapped().toString}")
    }
    mpks should have size 1
    mpks.asScala.head.getKey shouldBe "my_default_wallet"
    mpks.asScala.head.getValue.unwrapped().toString shouldBe "xpub661MyMwAqRbcGr3NH9q81huWmqC31HMwJ5PqDzHqGnYghQy9QgvxS86qZcBjJVCXbe2uvbP3nG7P8qKkeFp86AwS8vWzdbsoRXTimc7aAZj"
  }

  "configuration" should "contain gap_limit" in {
    val config = ConfigFactory.load
    val gapLimit = config.getInt("epsmi.gap-limit")
    gapLimit shouldBe 25
  }

}
