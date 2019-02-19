package com.zbsnetwork.http

import com.typesafe.config.ConfigFactory
import com.zbsnetwork.common.utils.Base58
import com.zbsnetwork.crypto
import com.zbsnetwork.settings.RestAPISettings

trait RestAPISettingsHelper {
  def apiKey: String = "test_api_key"

  lazy val MaxTransactionsPerRequest = 10000
  lazy val MaxAddressesPerRequest    = 10000

  lazy val restAPISettings = {
    val keyHash = Base58.encode(crypto.secureHash(apiKey.getBytes()))
    RestAPISettings.fromConfig(
      ConfigFactory
        .parseString(
          s"""
             |zbs.rest-api {
             |  api-key-hash = $keyHash
             |  transactions-by-address-limit = $MaxTransactionsPerRequest
             |  distribution-by-address-limit = $MaxAddressesPerRequest
             |}
           """.stripMargin
        )
        .withFallback(ConfigFactory.load()))
  }
}
