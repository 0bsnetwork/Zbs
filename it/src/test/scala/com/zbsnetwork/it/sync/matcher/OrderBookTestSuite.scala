package com.zbsnetwork.it.sync.matcher

import com.typesafe.config.Config
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.matcher.MatcherSuiteBase
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsnetwork.transaction.assets.exchange.Order.PriceConstant
import com.zbsnetwork.transaction.assets.exchange.OrderType._

class OrderBookTestSuite extends MatcherSuiteBase {

  override protected def nodeConfigs: Seq[Config] = Configs

  Seq(IssueUsdTx, IssueWctTx).map(createSignedIssueRequest).map(matcherNode.signedIssue).foreach { tx =>
    nodes.waitForTransaction(tx.id)
  }

  Seq(
    aliceNode.transfer(IssueUsdTx.sender.toAddress.stringRepr, aliceAcc.address, defaultAssetQuantity, 100000, Some(UsdId.toString), None, 2),
    bobNode.transfer(IssueWctTx.sender.toAddress.stringRepr, bobAcc.address, defaultAssetQuantity, 100000, Some(WctId.toString), None, 2)
  ).foreach { tx =>
    nodes.waitForTransaction(tx.id)
  }

  case class ReservedBalances(wct: Long, usd: Long, zbs: Long)
  def reservedBalancesOf(pk: PrivateKeyAccount): ReservedBalances = {
    val reservedBalances = matcherNode.reservedBalance(pk)
    ReservedBalances(
      reservedBalances.getOrElse(WctId.toString, 0),
      reservedBalances.getOrElse(UsdId.toString, 0),
      reservedBalances.getOrElse("ZBS", 0)
    )
  }

  val (amount, price) = (1000L, PriceConstant)

  "When delete order book" - {
    val buyOrder        = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, 2 * amount, price, matcherFee).message.id
    val anotherBuyOrder = matcherNode.placeOrder(aliceAcc, wctUsdPair, BUY, amount, price, matcherFee).message.id

    val submitted = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, price, matcherFee).message.id

    val sellOrder = matcherNode.placeOrder(bobAcc, wctUsdPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "PartiallyFilled")
    matcherNode.waitOrderStatus(wctUsdPair, submitted, "Filled")

    val (aliceRBForOnePair, bobRBForOnePair) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    val buyOrderForAnotherPair = matcherNode.placeOrder(aliceAcc, wctZbsPair, BUY, amount, price, matcherFee).message.id
    val sellOrderForAnotherPair =
      matcherNode.placeOrder(bobAcc, wctZbsPair, SELL, amount, 2 * price, matcherFee).message.id

    matcherNode.waitOrderStatus(wctZbsPair, buyOrderForAnotherPair, "Accepted")
    matcherNode.waitOrderStatus(wctZbsPair, sellOrderForAnotherPair, "Accepted")

    val (aliceRBForBothPairs, bobRBForBothPairs) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))

    matcherNode.deleteOrderBook(wctUsdPair)

    "orders by the pair should be canceled" in {
      matcherNode.waitOrderStatus(wctUsdPair, buyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, anotherBuyOrder, "Cancelled")
      matcherNode.waitOrderStatus(wctUsdPair, sellOrder, "Cancelled")
    }

    "orderbook was really deleted" in {
      val orderBook = matcherNode.orderBook(wctUsdPair)
      orderBook.bids shouldBe empty
      orderBook.asks shouldBe empty
    }

    "reserved balances should be released for the pair" in {
      val (aliceReservedBalances, bobReservedBalances) = (reservedBalancesOf(aliceAcc), reservedBalancesOf(bobAcc))
      aliceReservedBalances.usd shouldBe 0
      aliceReservedBalances.zbs shouldBe (aliceRBForBothPairs.zbs - aliceRBForOnePair.zbs)
      bobReservedBalances.wct shouldBe (bobRBForBothPairs.wct - bobRBForOnePair.wct)
      bobReservedBalances.zbs shouldBe (bobRBForBothPairs.zbs - bobRBForOnePair.zbs)
    }

    "it should not affect other pairs and their orders" in {
      matcherNode.orderStatus(buyOrderForAnotherPair, wctZbsPair).status shouldBe "Accepted"
      matcherNode.orderStatus(sellOrderForAnotherPair, wctZbsPair).status shouldBe "Accepted"

      val orderBook = matcherNode.orderBook(wctZbsPair)
      orderBook.bids shouldNot be(empty)
      orderBook.asks shouldNot be(empty)
    }
  }
}
