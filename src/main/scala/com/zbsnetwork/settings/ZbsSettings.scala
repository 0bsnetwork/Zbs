package com.zbsnetwork.settings

import com.typesafe.config.Config
import com.zbsnetwork.matcher.MatcherSettings
import com.zbsnetwork.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import scala.concurrent.duration._

case class ZbsSettings(directory: String,
                       dataDirectory: String,
                       maxCacheSize: Int,
                       maxRollbackDepth: Int,
                       rememberBlocks: FiniteDuration,
                       ntpServer: String,
                       networkSettings: NetworkSettings,
                       walletSettings: WalletSettings,
                       blockchainSettings: BlockchainSettings,
                       checkpointsSettings: CheckpointsSettings,
                       matcherSettings: MatcherSettings,
                       minerSettings: MinerSettings,
                       restAPISettings: RestAPISettings,
                       synchronizationSettings: SynchronizationSettings,
                       utxSettings: UtxSettings,
                       featuresSettings: FeaturesSettings,
                       metrics: Metrics.Settings)

object ZbsSettings {

  import NetworkSettings.networkSettingsValueReader

  val configPath: String = "zbs"

  def fromConfig(config: Config): ZbsSettings = {
    val directory               = config.as[String](s"$configPath.directory")
    val dataDirectory           = config.as[String](s"$configPath.data-directory")
    val maxCacheSize            = config.as[Int](s"$configPath.max-cache-size")
    val maxRollbackDepth        = config.as[Int](s"$configPath.max-rollback-depth")
    val rememberBlocks          = config.as[FiniteDuration](s"$configPath.remember-blocks-interval-in-cache")
    val ntpServer               = config.as[String](s"$configPath.ntp-server")
    val networkSettings         = config.as[NetworkSettings]("zbs.network")
    val walletSettings          = config.as[WalletSettings]("zbs.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val checkpointsSettings     = CheckpointsSettings.fromConfig(config)
    val matcherSettings         = MatcherSettings.fromConfig(config)
    val minerSettings           = MinerSettings.fromConfig(config)
    val restAPISettings         = RestAPISettings.fromConfig(config)
    val synchronizationSettings = SynchronizationSettings.fromConfig(config)
    val utxSettings             = config.as[UtxSettings]("zbs.utx")
    val featuresSettings        = config.as[FeaturesSettings]("zbs.features")
    val metrics                 = config.as[Metrics.Settings]("metrics")

    ZbsSettings(
      directory,
      dataDirectory,
      maxCacheSize,
      maxRollbackDepth,
      rememberBlocks,
      ntpServer,
      networkSettings,
      walletSettings,
      blockchainSettings,
      checkpointsSettings,
      matcherSettings,
      minerSettings,
      restAPISettings,
      synchronizationSettings,
      utxSettings,
      featuresSettings,
      metrics
    )
  }
}
