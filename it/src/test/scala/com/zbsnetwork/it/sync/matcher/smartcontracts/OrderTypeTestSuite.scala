package com.zbsnetwork.it.sync.matcher.smartcontracts

import com.typesafe.config.Config
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.api.http.TransactionNotAllowedByAccountScript
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.matcher.MatcherSuiteBase
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order, OrderType}
import play.api.libs.json.Json

import scala.concurrent.duration._

class OrderTypeTestSuite extends MatcherSuiteBase {
  override protected def nodeConfigs: Seq[Config] = Configs

  private val aliceAsset =
    aliceNode
      .issue(aliceAcc.address, "AliceCoinOrders", "AliceCoin for tests with order types", someAssetAmount, 0, reissuable = false, smartIssueFee, 2)
      .id

  {
    val issueTx = matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
    nodes.waitForTransaction(issueTx.id)

    val transferTx = aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2)
    nodes.waitForTransaction(transferTx.id)
  }

  private val predefAssetPair = zbsUsdPair
  private val aliceZbsPair  = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

  "Order types verification with SmartContracts" - {
    val sco1 = s"""
                 |{-# LANGUAGE_VERSION 2 #-}
                 |match tx {
                 | case o : Order =>
                 |   o.orderType == Buy
                 | case s : SetScriptTransaction => true
                 | case other => throw()
                 | }
                 |""".stripMargin

    val sco2 = s"""
              |{-# LANGUAGE_VERSION 2 #-}
              |match tx {
              | case o : Order =>
              |    o.orderType == Sell
              |  case s : SetScriptTransaction => true
              |  case _ => throw()
              | }
      """.stripMargin

    val sco3 = s"""
                 |{-# LANGUAGE_VERSION 2 #-}
                 |match tx {
                 |  case o : Order =>
                 |        o.orderType == Buy || o.orderType == Sell
                 |  case s : SetScriptTransaction => true
                 |  case _ => throw()
                 | }
      """.stripMargin

    "scenarios of order placement" - {
      "set contracts with only BUY type and then place order" in {
        setContract(Some(sco1), aliceAcc)

        val aliceOrd1 = matcherNode
          .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        assertBadRequest(
          matcherNode
            .placeOrder(aliceAcc, aliceZbsPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        matcherNode.cancelOrder(aliceAcc, predefAssetPair, aliceOrd1).status should be("OrderCanceled")

        setContract(None, aliceAcc)
      }

      "set contracts with only SELL type and then place order" in {
        setContract(Some(sco2), aliceAcc)

        assertBadRequest(
          matcherNode
            .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
            .message
            .id)

        val aliceOrd2 = matcherNode
          .placeOrder(aliceAcc, aliceZbsPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(aliceZbsPair, aliceOrd2, "Accepted", 1.minute)

        matcherNode.cancelOrder(aliceAcc, aliceZbsPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, aliceAcc)
      }

      "set contracts with both SELL/BUY types and then place order" in {
        setContract(Some(sco3), aliceAcc)

        val aliceOrd1 = matcherNode
          .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = matcherNode
          .placeOrder(aliceAcc, aliceZbsPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(aliceZbsPair, aliceOrd2, "Accepted", 1.minute)

        matcherNode.cancelOrder(aliceAcc, predefAssetPair, aliceOrd1).status should be("OrderCanceled")
        matcherNode.cancelOrder(aliceAcc, aliceZbsPair, aliceOrd2).status should be("OrderCanceled")

        setContract(None, aliceAcc)
      }

      "place order and then set contract on BUY type" in {
        val aliceOrd1 = matcherNode
          .placeOrder(aliceAcc, predefAssetPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Accepted", 1.minute)

        val aliceOrd2 = matcherNode
          .placeOrder(aliceAcc, aliceZbsPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 2, 10.minutes)
          .message
          .id
        matcherNode.waitOrderStatus(aliceZbsPair, aliceOrd2, "Accepted", 1.minute)

        setContract(Some(sco1), aliceAcc)

        val bobOrd1 = matcherNode
          .placeOrder(bobAcc, predefAssetPair, OrderType.SELL, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id
        val bobOrd2 = matcherNode
          .placeOrder(bobAcc, aliceZbsPair, OrderType.BUY, 500, 2.zbs * Order.PriceConstant, smartMatcherFee, version = 1, 10.minutes)
          .message
          .id

        matcherNode.waitOrderStatus(predefAssetPair, aliceOrd1, "Filled", 1.minute)
        matcherNode.waitOrderStatus(aliceZbsPair, aliceOrd2, "Filled", 1.minute)
        matcherNode.waitOrderStatus(predefAssetPair, bobOrd1, "Filled", 1.minute)
        matcherNode.waitOrderStatus(aliceZbsPair, bobOrd2, "Filled", 1.minute)

        val exchangeTx1 = matcherNode.transactionsByOrder(bobOrd1).headOption.getOrElse(fail("Expected an exchange transaction"))
        nodes.waitForTransaction(exchangeTx1.id)

        val txs = matcherNode.transactionsByOrder(bobOrd2)
        txs.size shouldBe 1
        matcherNode.expectSignedBroadcastRejected(Json.toJson(txs.head)) shouldBe TransactionNotAllowedByAccountScript.ErrorCode
      }
    }
  }
}
