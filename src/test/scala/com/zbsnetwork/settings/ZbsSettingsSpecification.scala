package com.zbsnetwork.settings

import java.io.File

import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

class ZbsSettingsSpecification extends FlatSpec with Matchers {
  private val home = System.getProperty("user.home")

  private def config(configName: String) =
    ZbsSettings.fromConfig(ConfigFactory.parseFile(new File(s"zbs-$configName.conf")).withFallback(ConfigFactory.load()))

  def testConfig(configName: String)(additionalChecks: ZbsSettings => Unit = _ => ()) {
    "ZbsSettings" should s"read values from default config with $configName overrides" in {
      val settings = config(configName)

      settings.directory should be(home + "/zbs")
      settings.networkSettings should not be null
      settings.walletSettings should not be null
      settings.blockchainSettings should not be null
      settings.checkpointsSettings should not be null
      settings.matcherSettings should not be null
      settings.minerSettings should not be null
      settings.restAPISettings should not be null
      settings.synchronizationSettings should not be null
      settings.utxSettings should not be null
      additionalChecks(settings)
    }
  }

  testConfig("mainnet")()
  testConfig("testnet")()
  testConfig("devnet")()

  "ZbsSettings" should "resolve folders correctly" in {
    val config = loadConfig(ConfigFactory.parseString(s"""zbs {
         |  directory = "/xxx"
         |  data-directory = "/xxx/data"
         |  ntp-server = "example.com"
         |}""".stripMargin))

    val settings = ZbsSettings.fromConfig(config.resolve())

    settings.directory should be("/xxx")
    settings.dataDirectory should be("/xxx/data")
    settings.ntpServer should be("example.com")
    settings.networkSettings.file should be(Some(new File("/xxx/peers.dat")))
    settings.walletSettings.file should be(Some(new File("/xxx/wallet/wallet.dat")))
    settings.matcherSettings.journalDataDir should be("/xxx/matcher/journal")
    settings.matcherSettings.snapshotsDataDir should be("/xxx/matcher/snapshots")
  }

}
