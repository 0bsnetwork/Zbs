package com.zbsplatform.settings

import com.typesafe.config.Config
import com.zbsplatform.matcher.MatcherSettings
import com.zbsplatform.metrics.Metrics
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._

case class ZbsSettings(directory: String,
                       dataDirectory: String,
                       maxCacheSize: Int,
                       networkSettings: NetworkSettings,
                       walletSettings: WalletSettings,
                       blockchainSettings: BlockchainSettings,
                       checkpointsSettings: CheckpointsSettings,
                       feesSettings: FeesSettings,
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
    val networkSettings         = config.as[NetworkSettings]("zbs.network")
    val walletSettings          = config.as[WalletSettings]("zbs.wallet")
    val blockchainSettings      = BlockchainSettings.fromConfig(config)
    val checkpointsSettings     = CheckpointsSettings.fromConfig(config)
    val feesSettings            = FeesSettings.fromConfig(config)
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
      networkSettings,
      walletSettings,
      blockchainSettings,
      checkpointsSettings,
      feesSettings,
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
