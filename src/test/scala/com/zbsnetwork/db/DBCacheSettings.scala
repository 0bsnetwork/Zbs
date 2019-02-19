package com.zbsnetwork.db
import com.typesafe.config.ConfigFactory
import com.zbsnetwork.settings.ZbsSettings

trait DBCacheSettings {
  lazy val maxCacheSize: Int = {
    val settings = ZbsSettings.fromConfig(ConfigFactory.load())
    settings.maxCacheSize
  }
}
