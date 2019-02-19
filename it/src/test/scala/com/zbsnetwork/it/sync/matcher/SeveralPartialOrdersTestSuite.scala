package com.zbsnetwork.it.sync.matcher

import com.typesafe.config.Config
import com.zbsnetwork.it.api.LevelResponse
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.matcher.MatcherSuiteBase
import com.zbsnetwork.it.sync.matcherFee
import com.zbsnetwork.transaction.assets.exchange.OrderType.BUY
import com.zbsnetwork.transaction.assets.exchange.{Order, OrderType}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import com.zbsnetwork.it.sync.matcher.config.MatcherPriceAssetConfig._

class SeveralPartialOrdersTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    matcherNode.waitForTransaction(matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx)).id)
    matcherNode.waitForTransaction(
      aliceNode.transfer(aliceNode.address, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2).id)
  }

  "Alice and Bob trade ZBS-USD" - {
    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place usd-zbs order" in {
      // Alice wants to sell USD for Zbs
      val bobZbsBalanceBefore = matcherNode.accountBalances(bobAcc.address)._1

      val bobOrder1   = matcherNode.prepareOrder(bobAcc, zbsUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobAcc)("ZBS") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobAcc, zbsUsdPair)("ZBS") shouldBe bobZbsBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceAcc, zbsUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = matcherNode.prepareOrder(aliceAcc, zbsUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder2Id, "Filled", 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(bobOrder1Id).headOption.getOrElse(fail("Expected an exchange transaction"))
      matcherNode.waitForTransaction(exchangeTx.id)
      matcherNode.reservedBalance(bobAcc) shouldBe empty
      matcherNode.reservedBalance(aliceAcc) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(zbsUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobAcc, zbsUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(zbsUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty

      matcherNode.cancelOrder(bobAcc, zbsUsdPair, bobOrder2Id)
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder2Id, "Cancelled", 1.minute)

      matcherNode.reservedBalance(bobAcc) shouldBe empty
      matcherNode.reservedBalance(aliceAcc) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1   = matcherNode.prepareOrder(aliceAcc, zbsUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder1Id = matcherNode.placeOrder(aliceOrder1).message.id

      val aliceOrder2   = matcherNode.prepareOrder(aliceAcc, zbsUsdPair, OrderType.BUY, buyOrderAmount, price)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id

      val bobOrder1   = matcherNode.prepareOrder(bobAcc, zbsUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id

      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder1Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder2Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTxs = matcherNode.transactionsByOrder(bobOrder1Id)
      exchangeTxs should not be empty
      exchangeTxs.map(_.id).foreach {
        matcherNode.waitForTransaction(_)
      }

      matcherNode.reservedBalance(bobAcc) shouldBe empty
      matcherNode.reservedBalance(aliceAcc) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(zbsUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobAcc, zbsUsdPair, OrderType.SELL, sellOrderAmount, price)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(zbsUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.amount, bobOrder2.price))
      orderBook2.bids shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchAmount: Long, matchPrice: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
