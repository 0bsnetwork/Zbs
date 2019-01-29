package com.zbsnetwork.settings

import com.typesafe.config.Config
import com.zbsnetwork.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader._
import net.ceedubs.ficus.readers.EnumerationReader._

import scala.concurrent.duration._

case class FunctionalitySettings(featureCheckBlocksPeriod: Int,
                                 blocksForFeatureActivation: Int,
                                 allowTemporaryNegativeUntil: Long,
                                 generationBalanceDepthFrom50To1000AfterHeight: Int,
                                 minimalGeneratingBalanceAfter: Long,
                                 allowTransactionsFromFutureUntil: Long,
                                 allowUnissuedAssetsUntil: Long,
                                 allowInvalidReissueInSameBlockUntilTimestamp: Long,
                                 allowMultipleLeaseCancelTransactionUntilTimestamp: Long,
                                 resetEffectiveBalancesAtHeight: Int,
                                 blockVersion3AfterHeight: Int,
                                 preActivatedFeatures: Map[Short, Int],
                                 doubleFeaturesPeriodsAfterHeight: Int,
                                 maxTransactionTimeBackOffset: FiniteDuration,
                                 maxTransactionTimeForwardOffset: FiniteDuration) {
  val allowLeasedBalanceTransferUntilHeight: Int = blockVersion3AfterHeight

  require(featureCheckBlocksPeriod > 0, "featureCheckBlocksPeriod must be greater than 0")
  require(
    (blocksForFeatureActivation > 0) && (blocksForFeatureActivation <= featureCheckBlocksPeriod),
    s"blocksForFeatureActivation must be in range 1 to $featureCheckBlocksPeriod"
  )

  def activationWindowSize(height: Int): Int =
    featureCheckBlocksPeriod * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def activationWindow(height: Int): Range =
    if (height < 1) Range(0, 0)
    else {
      val ws = activationWindowSize(height)
      Range.inclusive((height - 1) / ws * ws + 1, ((height - 1) / ws + 1) * ws)
    }

  def blocksForFeatureActivation(height: Int): Int =
    blocksForFeatureActivation * (if (height <= doubleFeaturesPeriodsAfterHeight) 1 else 2)

  def generatingBalanceDepth(height: Int): Int =
    if (height >= generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
}

object FunctionalitySettings {
  val MAINNET = apply(
    featureCheckBlocksPeriod = 5000,
    blocksForFeatureActivation = 4000,
    allowTemporaryNegativeUntil = 1479168000000L,
    generationBalanceDepthFrom50To1000AfterHeight = 232000,
    minimalGeneratingBalanceAfter = 1479168000000L,
    allowTransactionsFromFutureUntil = 1479168000000L,
    allowUnissuedAssetsUntil = 1479416400000L,
    allowInvalidReissueInSameBlockUntilTimestamp = 1492768800000L,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 1492768800000L,
    resetEffectiveBalancesAtHeight = 462000,
    blockVersion3AfterHeight = 795000,
    preActivatedFeatures = Map.empty,
    doubleFeaturesPeriodsAfterHeight = 810000,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val TESTNET = apply(
    featureCheckBlocksPeriod = 30,
    blocksForFeatureActivation = 25,
    allowTemporaryNegativeUntil = 0,
    generationBalanceDepthFrom50To1000AfterHeight = 0,
    minimalGeneratingBalanceAfter = 0,
    allowTransactionsFromFutureUntil = 0,
    allowUnissuedAssetsUntil = 0,
    allowInvalidReissueInSameBlockUntilTimestamp = 0,
    allowMultipleLeaseCancelTransactionUntilTimestamp = 0,
    resetEffectiveBalancesAtHeight = 1,
    blockVersion3AfterHeight = 0,
    preActivatedFeatures = Map[Short, Int]((1, 0), (2, 0), (3, 0), (4, 0), (5, 0), (6, 0), (7, 0), (8, 0)),
    doubleFeaturesPeriodsAfterHeight = Int.MaxValue,
    maxTransactionTimeBackOffset = 120.minutes,
    maxTransactionTimeForwardOffset = 90.minutes
  )

  val configPath = "zbs.blockchain.custom.functionality"
}

case class GenesisTransactionSettings(recipient: String, amount: Long)

case class GenesisSettings(blockTimestamp: Long,
                           timestamp: Long,
                           initialBalance: Long,
                           signature: Option[ByteStr],
                           transactions: Seq[GenesisTransactionSettings],
                           initialBaseTarget: Long,
                           averageBlockDelay: FiniteDuration)

object GenesisSettings {
  val MAINNET = GenesisSettings(
    1538689931932L,
    1478000000000L,
    Constants.UnitsInZbs * Constants.TotalZbs,
    ByteStr.decodeBase58("64dMbqe7XGomp7XU2SqfHGhUqGSwiQLD3TJCC4WMYPTAjkh2bprZJm9YT3mRxMqfMH5DwvnhdeyAm2FnUXYnUtm1").toOption,
    List(
      GenesisTransactionSettings("3Qbnb1eSHqztPMvR7qHhEQoGn6HAcLRe4Yz", (Constants.UnitsInZbs * Constants.TotalZbs * 0.5).toLong),
      GenesisTransactionSettings("3QE1Hju1y8CS3efwYwfikjUyABUd9BNfudc", (Constants.UnitsInZbs * Constants.TotalZbs * 0.5).toLong),
    ),
    153722867L,
    60.seconds
  )

  val TESTNET = GenesisSettings(
    1538694637902L,
    1538694637902L,
    Constants.UnitsInZbs * Constants.TotalZbs,
    ByteStr.decodeBase58("24scqU9MhmHHwwjKgAaNd1wiH39yWjtokFjeTxH8suHjw4Z37UPahu9BFYV9aqwsgsgpqBrBYYRD4RwF738XGnPR").toOption,
    List(
      GenesisTransactionSettings("3NBky6zeZav78TKabgmhKW3e4KFcx16LDVB", (Constants.UnitsInZbs * Constants.TotalZbs * 0.5).toLong),
      GenesisTransactionSettings("3MoyfqFEEs7enk572o9iqpjLSQT5UvxJMDw", (Constants.UnitsInZbs * Constants.TotalZbs * 0.5).toLong),
    ),
    153722867L,
    60.seconds
  )
}

case class BlockchainSettings(addressSchemeCharacter: Char, functionalitySettings: FunctionalitySettings, genesisSettings: GenesisSettings)

object BlockchainType extends Enumeration {
  val TESTNET = Value("TESTNET")
  val MAINNET = Value("MAINNET")
  val CUSTOM  = Value("CUSTOM")
}

object BlockchainSettings {
  val configPath: String = "zbs.blockchain"

  def fromConfig(config: Config): BlockchainSettings = {
    val blockchainType = config.as[BlockchainType.Value](s"$configPath.type")
    val (addressSchemeCharacter, functionalitySettings, genesisSettings) = blockchainType match {
      case BlockchainType.TESTNET =>
        ('T', FunctionalitySettings.TESTNET, GenesisSettings.TESTNET)
      case BlockchainType.MAINNET =>
        ('Z', FunctionalitySettings.MAINNET, GenesisSettings.MAINNET)
      case BlockchainType.CUSTOM =>
        val addressSchemeCharacter = config.as[String](s"$configPath.custom.address-scheme-character").charAt(0)
        val functionalitySettings  = config.as[FunctionalitySettings]("zbs.blockchain.custom.functionality")
        val genesisSettings        = config.as[GenesisSettings]("zbs.blockchain.custom.genesis")
        (addressSchemeCharacter, functionalitySettings, genesisSettings)
    }

    BlockchainSettings(
      addressSchemeCharacter = addressSchemeCharacter,
      functionalitySettings = functionalitySettings,
      genesisSettings = genesisSettings
    )
  }
}
