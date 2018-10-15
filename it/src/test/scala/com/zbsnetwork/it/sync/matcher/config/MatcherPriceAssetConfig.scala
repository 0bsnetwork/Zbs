package com.zbsplatform.it.sync.matcher.config

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.{empty, parseString}
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.api.http.assets.SignedIssueV1Request
import com.zbsplatform.it.NodeConfigs.Default
import com.zbsplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.zbsplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsplatform.it.util._
import com.zbsplatform.transaction.assets.IssueTransactionV1
import com.zbsplatform.transaction.assets.exchange.AssetPair
import com.zbsplatform.utils.Base58

import scala.util.Random

object MatcherPriceAssetConfig {

  private val _Configs: Seq[Config] = (Default.last +: Random.shuffle(Default.init).take(3))
    .zip(Seq(matcherConfig, minerDisabled, minerDisabled, empty()))
    .map { case (n, o) => o.withFallback(n) }

  private val aliceSeed = _Configs(1).getString("account-seed")
  private val bobSeed   = _Configs(2).getString("account-seed")
  private val alicePk   = PrivateKeyAccount.fromSeed(aliceSeed).right.get
  private val bobPk     = PrivateKeyAccount.fromSeed(bobSeed).right.get

  val Decimals: Byte = 2

  val usdAssetName = "USD-X"
  val wctAssetName = "WCT-X"
  val ethAssetName = "ETH-X"

  val IssueUsdTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = usdAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.zbs,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueWctTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = wctAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = Decimals,
      reissuable = false,
      fee = 1.zbs,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueEthTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = bobPk,
      name = ethAssetName.getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.zbs,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val IssueBtcTx: IssueTransactionV1 = IssueTransactionV1
    .selfSigned(
      sender = alicePk,
      name = "BTC-X".getBytes(),
      description = "asset description".getBytes(),
      quantity = defaultAssetQuantity,
      decimals = 8,
      reissuable = false,
      fee = 1.zbs,
      timestamp = System.currentTimeMillis()
    )
    .right
    .get

  val BtcId = IssueBtcTx.id()
  val EthId = IssueEthTx.id()
  val UsdId = IssueUsdTx.id()
  val WctId = IssueWctTx.id()

  val wctUsdPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = Some(UsdId)
  )

  val wctZbsPair = AssetPair(
    amountAsset = Some(WctId),
    priceAsset = None
  )

  val ethZbsPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = None
  )

  val ethBtcPair = AssetPair(
    amountAsset = Some(EthId),
    priceAsset = Some(BtcId)
  )

  val zbsUsdPair = AssetPair(
    amountAsset = None,
    priceAsset = Some(UsdId)
  )

  val orderLimit = 10

  private val updatedMatcherConfig = parseString(s"""
                                                    |zbs.matcher {
                                                    |  price-assets = [ "$UsdId", "$BtcId", "ZBS" ]
                                                    |  rest-order-limit=$orderLimit
                                                    |}
     """.stripMargin)

  val Configs: Seq[Config] = _Configs.map(updatedMatcherConfig.withFallback(_))

  def createSignedIssueRequest(tx: IssueTransactionV1): SignedIssueV1Request = {
    import tx._
    SignedIssueV1Request(
      Base58.encode(tx.sender.publicKey),
      new String(name),
      new String(description),
      quantity,
      decimals,
      reissuable,
      fee,
      timestamp,
      signature.base58
    )
  }

}
