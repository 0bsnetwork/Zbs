package com.zbsplatform.it.sync.matcher

import com.typesafe.config.Config
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.LevelResponse
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsplatform.it.sync.matcherFee
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.transaction.assets.exchange.OrderType.BUY
import com.zbsplatform.transaction.assets.exchange.{Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

class SeveralPartialOrdersTestSuite
    extends FreeSpec
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure
    with NodesFromDocker
    with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  nodes.waitForHeightArise()

  "Alice and Bob trade ZBS-USD" - {
    val bobZbsBalanceBefore = matcherNode.accountBalances(bobNode.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 840340L

    "place two submitted orders and one counter" in {
      val bobOrder1   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobNode)("ZBS") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS") shouldBe bobZbsBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder1  = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder1).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrderId, "Filled", 1.minute)

      val aliceOrder2   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder2Id, "Filled", 1.minute)

      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(bobOrder1Id).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(zbsUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(zbsUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.price, bobOrder2.amount))
      orderBook2.bids shouldBe empty

      matcherNode.cancelOrder(bobNode, zbsUsdPair, Some(bobOrder2Id))
      nodes.waitForHeightArise()

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    "place one submitted orders and two counter" in {
      val aliceOrder1   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrder1Id = matcherNode.placeOrder(aliceOrder1).message.id

      val aliceOrder2   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrder2Id = matcherNode.placeOrder(aliceOrder2).message.id

      val bobOrder1   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id

      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder1Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrder2Id, "Filled", 1.minute)
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Filled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTxs = matcherNode.transactionsByOrder(bobOrder1Id)
      exchangeTxs should not be empty
      exchangeTxs.map(_.id).foreach(nodes.waitForHeightAriseAndTxPresent)

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty

      // Previously cancelled order should not affect new orders
      val orderBook1 = matcherNode.orderBook(zbsUsdPair)
      orderBook1.asks shouldBe empty
      orderBook1.bids shouldBe empty

      val bobOrder2   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder2Id = matcherNode.placeOrder(bobOrder2).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder2Id, "Accepted", 1.minute)

      val orderBook2 = matcherNode.orderBook(zbsUsdPair)
      orderBook2.asks shouldBe List(LevelResponse(bobOrder2.price, bobOrder2.amount))
      orderBook2.bids shouldBe empty
    }
  }

  def correctAmount(a: Long, price: Long): Long = {
    val settledTotal = (BigDecimal(price) * a / Order.PriceConstant).setScale(0, RoundingMode.FLOOR).toLong
    (BigDecimal(settledTotal) / price * Order.PriceConstant).setScale(0, RoundingMode.CEILING).toLong
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
