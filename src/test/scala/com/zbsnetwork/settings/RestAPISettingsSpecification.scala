package com.zbsnetwork.settings

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class RestAPISettingsSpecification extends FlatSpec with Matchers {
  "RestAPISettings" should "read values" in {
    val config   = ConfigFactory.parseString("""
        |zbs {
        |  rest-api {
        |    enable: yes
        |    bind-address: "127.0.0.1"
        |    port: 7441
        |    api-key-hash: "BASE58APIKEYHASH"
        |    cors: yes
        |    api-key-different-host: yes
        |    transactions-by-address-limit = 10000
        |    distribution-address-limit = 10000
        |  }
        |}
      """.stripMargin)
    val settings = RestAPISettings.fromRootConfig(config)

    settings.enable should be(true)
    settings.bindAddress should be("127.0.0.1")
    settings.port should be(7441)
    settings.apiKeyHash should be("BASE58APIKEYHASH")
    settings.cors should be(true)
    settings.apiKeyDifferentHost should be(true)
    settings.transactionsByAddressLimit shouldBe 10000
    settings.distributionAddressLimit shouldBe 10000
  }

}
