package com.zbsnetwork.it.sync.matcher.smartcontracts

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.matcher.MatcherSuiteBase
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.concurrent.duration._

class OrdersFromScriptedAccTestSuite extends MatcherSuiteBase {

  import OrdersFromScriptedAccTestSuite._

  override protected def nodeConfigs: Seq[Config] = updatedConfigs

  private val sDupNames =
    """let x = (let x = 2
      |3)
      |x == 3""".stripMargin

  "issue asset and run test" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)
    val aliceZbsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    "setScript at account" in {
      // check assets's balances
      matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
      matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)

      withClue("mining was too fast, can't continue") {
        matcherNode.height shouldBe <(activationHeight)
      }

      setContract(Some("true"), bobAcc)
    }

    "trading is deprecated" in {
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceZbsPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartTradeFee, version = 1, 10.minutes),
        "Trading on scripted account isn't allowed yet"
      )
    }

    "can't place an OrderV2 before the activation" in {
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceZbsPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes),
        "Orders of version 1 are only accepted, because SmartAccountTrading has not been activated yet"
      )
    }

    "invalid setScript at account" in {
      matcherNode.waitForHeight(activationHeight, 6.minutes)
      setContract(Some("true && (height > 0)"), bobAcc)
      assertBadRequestAndResponse(
        matcherNode.placeOrder(bobAcc, aliceZbsPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes),
        "height is inaccessible when running script on matcher"
      )
    }

    "scripted account can trade once SmartAccountTrading is activated" in {
      setContract(Some(sDupNames), bobAcc)
      val bobOrder =
        matcherNode.placeOrder(bobAcc, aliceZbsPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartTradeFee, version = 2, 10.minutes)
      bobOrder.status shouldBe "OrderAccepted"
    }

    "can trade from non-scripted account" in {
      // Alice places sell order
      val aliceOrder =
        matcherNode.placeOrder(aliceAcc, aliceZbsPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, matcherFee, version = 1, 10.minutes)

      aliceOrder.status shouldBe "OrderAccepted"

      val orderId = aliceOrder.message.id
      // Alice checks that the order in order book
      matcherNode.waitOrderStatus(aliceZbsPair, orderId, "Filled")
      matcherNode.fullOrderHistory(aliceAcc).head.status shouldBe "Filled"
    }
  }
}

object OrdersFromScriptedAccTestSuite {
  val activationHeight = 25

  private val matcherConfig = ConfigFactory.parseString(s"""
                                                           |zbs {
                                                           |  blockchain.custom.functionality.pre-activated-features = {
                                                           |    ${BlockchainFeatures.SmartAccountTrading.id} = $activationHeight,
                                                           |    ${BlockchainFeatures.SmartAssets.id} = 1000
                                                           |  }
                                                           |}""".stripMargin)

  private val updatedConfigs: Seq[Config] = Configs.map(matcherConfig.withFallback(_))
}
