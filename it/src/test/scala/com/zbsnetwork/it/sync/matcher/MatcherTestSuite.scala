package com.zbsplatform.it.sync.matcher

import com.typesafe.config.Config
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.api.{AssetDecimalsInfo, LevelResponse}
import com.zbsplatform.it.sync._
import com.zbsplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.util._
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class MatcherTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  private val aliceSellAmount         = 500
  private val TransactionFee          = 300000
  private val amountAssetName         = "AliceCoin"
  private val AssetQuantity           = 1000
  private val aliceCoinDecimals: Byte = 0

  "Check cross ordering between Alice and Bob " - {
    // Alice issues new asset

    val aliceAsset = aliceNode
      .issue(aliceNode.address, amountAssetName, "AliceCoin for matcher's tests", AssetQuantity, aliceCoinDecimals, reissuable = false, 100000000L)
      .id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    val aliceZbsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    val order1 = matcherNode.placeOrder(aliceNode, aliceZbsPair, OrderType.SELL, 2.zbs * Order.PriceConstant, aliceSellAmount, 2.minutes)

    "assert addresses balances" in {
      aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, AssetQuantity)
      matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
      bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)
    }

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "get opened trading markets" in {
      val openMarkets = matcherNode.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe amountAssetName
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(aliceCoinDecimals))

      markets.priceAssetName shouldBe "ZBS"
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(8))
    }

    "sell order could be placed correctly" - {
      "alice places sell order" in {
        order1.status shouldBe "OrderAccepted"

        // Alice checks that the order in order book
        matcherNode.waitOrderStatus(aliceZbsPair, order1.message.id, "Accepted")

        // Alice check that order is correct
        val orders = matcherNode.orderBook(aliceZbsPair)
        orders.asks.head.amount shouldBe aliceSellAmount
        orders.asks.head.price shouldBe 2.zbs * Order.PriceConstant
      }

      "frozen amount should be listed via matcherBalance REST endpoint" in {
        matcherNode.reservedBalance(aliceNode) shouldBe Map(aliceAsset -> aliceSellAmount)

        matcherNode.reservedBalance(bobNode) shouldBe Map()
      }

      "and should be listed by trader's publiс key via REST" in {
        matcherNode.fullOrderHistory(aliceNode).map(_.id) should contain(order1.message.id)
      }

      "and should match with buy order" in {
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1

        // Bob places a buy order
        val order2 = matcherNode.placeOrder(bobNode, aliceZbsPair, OrderType.BUY, 2.zbs * Order.PriceConstant, 200)
        order2.status shouldBe "OrderAccepted"

        matcherNode.waitOrderStatus(aliceZbsPair, order1.message.id, "PartiallyFilled")
        matcherNode.waitOrderStatus(aliceZbsPair, order2.message.id, "Filled")

        matcherNode.orderHistoryByPair(bobNode, aliceZbsPair).map(_.id) should contain(order2.message.id)
        matcherNode.fullOrderHistory(bobNode).map(_.id) should contain(order2.message.id)

        nodes.waitForHeightArise()

        // Bob checks that asset on his balance
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 200)

        // Alice checks that part of her order still in the order book
        val orders = matcherNode.orderBook(aliceZbsPair)
        orders.asks.head.amount shouldBe 300
        orders.asks.head.price shouldBe 2.zbs * Order.PriceConstant

        // Alice checks that she sold some assets
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 800)

        // Bob checks that he spent some Zbs
        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance shouldBe (bobBalance - 2.zbs * 200 - matcherFee)

        // Alice checks that she received some Zbs
        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance shouldBe (aliceBalance + 2.zbs * 200 - (matcherFee * 200.0 / 500.0).toLong)

        // Matcher checks that it earn fees
        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance shouldBe (matcherBalance + matcherFee + (matcherFee * 200.0 / 500.0).toLong - TransactionFee)
      }

      "request activeOnly orders" in {
        val aliceOrders = matcherNode.activeOrderHistory(aliceNode)
        aliceOrders.map(_.id) shouldBe Seq(order1.message.id)
        val bobOrders = matcherNode.activeOrderHistory(bobNode)
        bobOrders.map(_.id) shouldBe Seq()
      }

      "submitting sell orders should check availability of asset" in {
        // Bob trying to place order on more assets than he has - order rejected
        val badOrder = matcherNode.prepareOrder(bobNode, aliceZbsPair, OrderType.SELL, (19.zbs / 10.0 * Order.PriceConstant).toLong, 300)
        matcherNode.expectIncorrectOrderPlacement(badOrder, 400, "OrderRejected") should be(true)

        // Bob places order on available amount of assets - order accepted
        val order3 = matcherNode.placeOrder(bobNode, aliceZbsPair, OrderType.SELL, (19.zbs / 10.0 * Order.PriceConstant).toLong, 150)
        order3.status should be("OrderAccepted")

        // Bob checks that the order in the order book
        val orders = matcherNode.orderBook(aliceZbsPair)
        orders.asks should contain(LevelResponse(19.zbs / 10 * Order.PriceConstant, 150))
      }

      "buy order should match on few price levels" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1

        // Alice places a buy order
        val order4 = matcherNode.placeOrder(aliceNode, aliceZbsPair, OrderType.BUY, (21.zbs / 10.0 * Order.PriceConstant).toLong, 350)
        order4.status should be("OrderAccepted")

        // Where were 2 sells that should fulfill placed order
        matcherNode.waitOrderStatus(aliceZbsPair, order4.message.id, "Filled")

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 950)
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 50)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(
          matcherBalance - 2 * TransactionFee + matcherFee + (matcherFee * 150.0 / 350.0).toLong + (matcherFee * 200.0 / 350.0).toLong + (matcherFee * 200.0 / 500.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance should be(bobBalance - matcherFee + 150 * (19.zbs / 10.0).toLong)

        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance should be(
          aliceBalance - (matcherFee * 200.0 / 350.0).toLong - (matcherFee * 150.0 / 350.0).toLong - (matcherFee * 200.0 / 500.0).toLong - (19.zbs / 10.0).toLong * 150)
      }

      "order could be canceled and resubmitted again" in {
        // Alice cancels the very first order (100 left)
        val status1 = matcherNode.cancelOrder(aliceNode, aliceZbsPair, Some(order1.message.id))
        status1.status should be("OrderCanceled")

        // Alice checks that the order book is empty
        val orders1 = matcherNode.orderBook(aliceZbsPair)
        orders1.asks.size should be(0)
        orders1.bids.size should be(0)

        // Alice places a new sell order on 100
        val order4 =
          matcherNode.placeOrder(aliceNode, aliceZbsPair, OrderType.SELL, 2.zbs * Order.PriceConstant, 100)
        order4.status should be("OrderAccepted")

        // Alice checks that the order is in the order book
        val orders2 = matcherNode.orderBook(aliceZbsPair)
        orders2.asks should contain(LevelResponse(20.zbs / 10 * Order.PriceConstant, 100))
      }

      "buy order should execute all open orders and put remaining in order book" in {
        val matcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        val aliceBalance   = aliceNode.accountBalances(aliceNode.address)._1
        val bobBalance     = bobNode.accountBalances(bobNode.address)._1

        // Bob places buy order on amount bigger then left in sell orders
        val order5 = matcherNode.placeOrder(bobNode, aliceZbsPair, OrderType.BUY, 2.zbs * Order.PriceConstant, 130)
        order5.status should be("OrderAccepted")

        // Check that the order is partially filled
        matcherNode.waitOrderStatus(aliceZbsPair, order5.message.id, "PartiallyFilled")

        // Check that remaining part of the order is in the order book
        val orders = matcherNode.orderBook(aliceZbsPair)
        orders.bids should contain(LevelResponse(2.zbs * Order.PriceConstant, 30))

        // Check balances
        nodes.waitForHeightArise()
        aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, 850)
        bobNode.assertAssetBalance(bobNode.address, aliceAsset, 150)

        val updatedMatcherBalance = matcherNode.accountBalances(matcherNode.address)._1
        updatedMatcherBalance should be(matcherBalance - TransactionFee + matcherFee + (matcherFee * 100.0 / 130.0).toLong)

        val updatedBobBalance = bobNode.accountBalances(bobNode.address)._1
        updatedBobBalance should be(bobBalance - (matcherFee * 100.0 / 130.0).toLong - 100 * 2.zbs)

        val updatedAliceBalance = aliceNode.accountBalances(aliceNode.address)._1
        updatedAliceBalance should be(aliceBalance - matcherFee + 2.zbs * 100)
      }

      "request order book for blacklisted pair" in {
        val f = matcherNode.matcherGetStatusCode(s"/matcher/orderbook/$ForbiddenAssetId/ZBS", 404)
        f.message shouldBe s"Invalid Asset ID: $ForbiddenAssetId"
      }

      "should consider UTX pool when checking the balance" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin"
        val bobAsset         = bobNode.issue(bobNode.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        aliceNode.assertAssetBalance(aliceNode.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherNode.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobNode.address, bobAsset, bobAssetQuantity)
        val bobZbsPair = AssetPair(ByteStr.decodeBase58(bobAsset).toOption, None)

        def bobOrder = matcherNode.prepareOrder(bobNode, bobZbsPair, OrderType.SELL, 1.zbs * Order.PriceConstant, bobAssetQuantity)

        val order6 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobZbsPair, order6.message.id, "Accepted")

        // Alice wants to buy all Bob's assets for 1 Zbs
        val order7 =
          matcherNode.placeOrder(aliceNode, bobZbsPair, OrderType.BUY, 1.zbs * Order.PriceConstant, bobAssetQuantity)
        matcherNode.waitOrderStatus(bobZbsPair, order7.message.id, "Filled")

        // Bob tries to do the same operation, but at now he have no assets
        matcherNode.expectIncorrectOrderPlacement(bobOrder, 400, "OrderRejected")
      }

      "trader can buy zbs for assets with order without having zbs" in {
        // Bob issues new asset
        val bobAssetQuantity = 10000
        val bobAssetName     = "BobCoin2"
        val bobAsset         = bobNode.issue(bobNode.address, bobAssetName, "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
        nodes.waitForHeightAriseAndTxPresent(bobAsset)

        val bobZbsPair = AssetPair(
          amountAsset = ByteStr.decodeBase58(bobAsset).toOption,
          priceAsset = None
        )

        aliceNode.assertAssetBalance(aliceNode.address, bobAsset, 0)
        matcherNode.assertAssetBalance(matcherNode.address, bobAsset, 0)
        bobNode.assertAssetBalance(bobNode.address, bobAsset, bobAssetQuantity)

        // Bob wants to sell all own assets for 1 Zbs
        def bobOrder = matcherNode.prepareOrder(bobNode, bobZbsPair, OrderType.SELL, 1.zbs * Order.PriceConstant, bobAssetQuantity)

        val order8 = matcherNode.placeOrder(bobOrder)
        matcherNode.waitOrderStatus(bobZbsPair, order8.message.id, "Accepted")

        // Bob moves all zbs to Alice
        val h1              = matcherNode.height
        val bobBalance      = matcherNode.accountBalances(bobNode.address)._1
        val transferAmount  = bobBalance - TransactionFee
        val transferAliceId = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
        nodes.waitForHeightAriseAndTxPresent(transferAliceId)

        matcherNode.accountBalances(bobNode.address)._1 shouldBe 0

        // Order should stay accepted
        matcherNode.waitForHeight(h1 + 5, 2.minutes)
        matcherNode.waitOrderStatus(bobZbsPair, order8.message.id, "Accepted")

        // Cleanup
        nodes.waitForHeightArise()
        matcherNode.cancelOrder(bobNode, bobZbsPair, Some(order8.message.id)).status should be("OrderCanceled")

        val transferBobId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
        nodes.waitForHeightAriseAndTxPresent(transferBobId)
      }
    }

    "batch cancel" - {
      val ordersNum = 5
      def fileOrders(n: Int, pair: AssetPair): Seq[String] = 0 until n map { _ =>
        val o = matcherNode.placeOrder(matcherNode.prepareOrder(aliceNode, pair, OrderType.BUY, 1.zbs * Order.PriceConstant, 100))
        o.status should be("OrderAccepted")
        o.message.id
      }

      val asset2 =
        aliceNode.issue(aliceNode.address, "AliceCoin2", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
      nodes.waitForHeightAriseAndTxPresent(asset2)
      val aliceZbsPair2 = AssetPair(ByteStr.decodeBase58(asset2).toOption, None)

      "canceling an order doesn't affect other orders for the same pair" in {
        val orders                          = fileOrders(ordersNum, aliceZbsPair)
        val (orderToCancel, ordersToRetain) = (orders.head, orders.tail)

        val cancel = matcherNode.cancelOrder(aliceNode, aliceZbsPair, Some(orderToCancel))
        cancel.status should be("OrderCanceled")

        ordersToRetain foreach {
          matcherNode.waitOrderStatus(aliceZbsPair, _, "Accepted")
        }
      }

      "cancel orders by pair" ignore {
        val ordersToCancel = fileOrders(orderLimit + ordersNum, aliceZbsPair)
        val ordersToRetain = fileOrders(ordersNum, aliceZbsPair2)
        val ts             = Some(System.currentTimeMillis)

        val cancel = matcherNode.cancelOrder(aliceNode, aliceZbsPair, None, ts)
        cancel.status should be("Cancelled")

        ordersToCancel foreach {
          matcherNode.waitOrderStatus(aliceZbsPair, _, "Cancelled")
        }
        ordersToRetain foreach {
          matcherNode.waitOrderStatus(aliceZbsPair2, _, "Accepted")
        }

        // signed timestamp is mandatory
        assertBadRequestAndMessage(matcherNode.cancelOrder(aliceNode, aliceZbsPair, None, None), "invalid signature")

        // timestamp reuse shouldn't be allowed
        assertBadRequest(matcherNode.cancelOrder(aliceNode, aliceZbsPair, None, ts))
      }

      "cancel all orders" ignore {
        val orders1 = fileOrders(orderLimit + ordersNum, aliceZbsPair)
        val orders2 = fileOrders(orderLimit + ordersNum, aliceZbsPair2)
        val ts      = Some(System.currentTimeMillis)

        val cancel = matcherNode.cancelAllOrders(aliceNode, ts)
        cancel.status should be("Cancelled")

        orders1 foreach {
          matcherNode.waitOrderStatus(aliceZbsPair, _, "Cancelled")
        }
        orders2 foreach {
          matcherNode.waitOrderStatus(aliceZbsPair2, _, "Cancelled")
        }

        // signed timestamp is mandatory
        assertBadRequestAndMessage(matcherNode.cancelAllOrders(aliceNode, None), "invalid signature")

        // timestamp reuse shouldn't be allowed
        assertBadRequest(matcherNode.cancelAllOrders(aliceNode, ts))
      }
    }
  }
}
