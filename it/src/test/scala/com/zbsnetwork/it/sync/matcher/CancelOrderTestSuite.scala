package com.zbsplatform.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.api.http.assets.SignedIssueV1Request
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.util._
import com.zbsplatform.transaction.AssetId
import com.zbsplatform.transaction.assets.IssueTransactionV1
import com.zbsplatform.transaction.assets.exchange.OrderType.BUY
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import com.zbsplatform.utils.Base58
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode
import scala.util.{Random, Try}
import com.zbsplatform.it.sync.matcher.config.MatcherPriceAssetConfig._

class CancelOrderTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode = nodes.head

  private def aliceNode = nodes(1)

  private def bobNode = nodes(2)

  matcherNode.signedIssue(createSignedIssueRequest(IssueUsdTx))
  matcherNode.signedIssue(createSignedIssueRequest(IssueWctTx))
  nodes.waitForHeightArise()

  "cancel order using api-key" in {
    val orderId = matcherNode.placeOrder(bobNode, zbsUsdPair, OrderType.SELL, 800, 100.zbs).message.id
    matcherNode.waitOrderStatus(zbsUsdPair, orderId, "Accepted", 1.minute)

    matcherNode.cancelOrderWithApiKey(orderId)
    matcherNode.waitOrderStatus(zbsUsdPair, orderId, "Cancelled", 1.minute)

    matcherNode.fullOrderHistory(bobNode).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderHistoryByPair(bobNode, zbsUsdPair).filter(_.id == orderId).head.status shouldBe "Cancelled"
    matcherNode.orderBook(zbsUsdPair).bids shouldBe empty
    matcherNode.orderBook(zbsUsdPair).asks shouldBe empty

    matcherNode.deleteOrder(bobNode, zbsUsdPair, Some(orderId))
    matcherNode.orderStatus(orderId, zbsUsdPair, false).status shouldBe "NotFound"

  }

  "Alice and Bob trade ZBS-USD" - {
    "place usd-zbs order" in {
      // Alice wants to sell USD for Zbs
      val orderId1      = matcherNode.placeOrder(bobNode, zbsUsdPair, OrderType.SELL, 800, 100.zbs).message.id
      val orderId2      = matcherNode.placeOrder(bobNode, zbsUsdPair, OrderType.SELL, 700, 100.zbs).message.id
      val bobSellOrder3 = matcherNode.placeOrder(bobNode, zbsUsdPair, OrderType.SELL, 600, 100.zbs).message.id

      matcherNode.fullOrderHistory(aliceNode)
      matcherNode.fullOrderHistory(bobNode)

      matcherNode.waitOrderStatus(zbsUsdPair, bobSellOrder3, "Accepted", 1.minute)

      val aliceOrder = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, 800, 0.00125.zbs)
      matcherNode.placeOrder(aliceOrder).message.id

      Thread.sleep(2000)
      matcherNode.fullOrderHistory(aliceNode)
      val orders = matcherNode.fullOrderHistory(bobNode)
      for (orderId <- Seq(orderId1, orderId2)) {
        orders.filter(_.id == orderId).head.status shouldBe "Accepted"
      }
    }

  }

  def correctAmount(a: Long, price: Long): Long = {
    val min = (BigDecimal(Order.PriceConstant) / price).setScale(0, RoundingMode.CEILING)
    if (min > 0)
      Try(((BigDecimal(a) / min).toBigInt() * min.toBigInt()).bigInteger.longValueExact()).getOrElse(Long.MaxValue)
    else
      a
  }

  def receiveAmount(ot: OrderType, matchPrice: Long, matchAmount: Long): Long =
    if (ot == BUY) correctAmount(matchAmount, matchPrice)
    else {
      (BigInt(matchAmount) * matchPrice / Order.PriceConstant).bigInteger.longValueExact()
    }

}
