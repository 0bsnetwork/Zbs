package com.zbsplatform.http

import com.typesafe.config.ConfigFactory
import com.zbsplatform.crypto
import com.zbsplatform.settings.RestAPISettings
import com.zbsplatform.utils.Base58

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(s"zbs.rest-api.api-key-hash = $keyHash")
        .withFallback(ConfigFactory.load()))
  }
}
