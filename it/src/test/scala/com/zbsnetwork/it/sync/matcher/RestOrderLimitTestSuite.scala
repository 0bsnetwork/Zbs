package com.zbsnetwork.it.sync.matcher

import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory.parseString
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsnetwork.it.sync.{someAssetAmount, _}
import com.zbsnetwork.it.transactions.NodesFromDocker
import com.zbsnetwork.it.util._
import com.zbsnetwork.it.{Node, ReportingTestName}
import com.zbsnetwork.common.state.ByteStr.decodeBase58
import com.zbsnetwork.transaction.assets.exchange.AssetPair
import com.zbsnetwork.transaction.assets.exchange.OrderType.{BUY, SELL}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class RestOrderLimitTestSuite
    extends FreeSpec
    with NodesFromDocker
    with ReportingTestName
    with Matchers
    with BeforeAndAfterAll
    with CancelAfterFailure {
  import RestOrderLimitTestSuite._
  override protected def nodeConfigs: Seq[Config] = CustomConfigs

  private def matcher = nodes.head
  private def alice   = nodes(1)
  private def bob     = nodes(2)

  private def activeOrders: Seq[String] = {
    val activeOrders = matcher.activeOrderHistory(alice.privateKey).map(_.id)
    matcher.ordersByAddress(alice.privateKey, activeOnly = true).map(_.id) should equal(activeOrders)
    activeOrders
  }

  private def allOrders: Seq[String] = {
    val allOrders = matcher.fullOrderHistory(alice.privateKey).map(_.id)
    matcher.ordersByAddress(alice.privateKey, activeOnly = false).map(_.id) should equal(allOrders)
    allOrders
  }

  private def activeOrdersBy(pair: AssetPair, n: Node = alice): Seq[String] =
    matcher.orderHistoryByPair(n.privateKey, pair, activeOnly = true).map(_.id)

  private def allOrdersBy(pair: AssetPair, n: Node = alice): Seq[String] = matcher.orderHistoryByPair(n.privateKey, pair).map(_.id)

  markup("""Test suite checks only Alice's OrderHistory.
          |Bob places orders only for matching Alice's orders.""".stripMargin)

  "Order History REST API methods should have limit for orders in response" in {
    val aliceAsset = alice
      .issue(alice.address, "AliceCoin", "Test", someAssetAmount, 0, reissuable = false, issueFee)
      .id
    val bobAsset = bob
      .issue(bob.address, "BobCoin", "Test", someAssetAmount, 0, reissuable = false, issueFee)
      .id
    Seq(aliceAsset, bobAsset).foreach(matcher.waitForTransaction(_))

    val alicePair = AssetPair(decodeBase58(aliceAsset).toOption, None)
    val bobPair   = AssetPair(decodeBase58(bobAsset).toOption, None)

    info("'fullOrderHistory' and 'ordersByAddress' (activeOnly=false) must return no more 'rest-order-limit' orders")

    val aliceAcc = alice.privateKey
    val bobAcc   = bob.privateKey

    val active0 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 15.zbs, matcherFee).message.id

    val active1    = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val partial1   = matcher.placeOrder(aliceAcc, alicePair, SELL, 2, 9.zbs, matcherFee).message.id
    val filled1    = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 8.zbs, matcherFee).message.id
    val cancelled1 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 11.zbs, matcherFee).message.id
    val active2    = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 2.zbs, matcherFee).message.id
    val filled2    = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 4.zbs, matcherFee).message.id
    val partial2   = matcher.placeOrder(aliceAcc, bobPair, BUY, 2, 3.zbs, matcherFee).message.id
    val cancelled2 = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 2.zbs, matcherFee).message.id

    // orders for matching Alice's orders
    matcher.placeOrder(bobAcc, alicePair, BUY, 1, 8.zbs, matcherFee).message.id // fill filled1
    matcher.placeOrder(bobAcc, alicePair, BUY, 1, 9.zbs, matcherFee).message.id // part fill partial1
    matcher.placeOrder(bobAcc, bobPair, SELL, 1, 4.zbs, matcherFee).message.id  // fill filled2
    matcher.placeOrder(bobAcc, bobPair, SELL, 1, 3.zbs, matcherFee).message.id  // part fill partial2

    matcher.cancelOrder(aliceAcc, alicePair, cancelled1)
    matcher.cancelOrder(aliceAcc, alicePair, cancelled2)
    matcher.waitOrderStatus(bobPair, cancelled2, "Cancelled", 1.minutes)

    val activeOrdersAllFive       = Seq(partial2, active2, partial1, active1, active0)
    val allOrdersExceptTheFilled1 = activeOrdersAllFive ++ Seq(cancelled2, filled2, cancelled1)
    val activeOrdersByPair        = Seq(partial1, active1, active0)
    val allOrdersByPair           = activeOrdersByPair ++ Seq(cancelled1, filled1)

    activeOrders should equal(activeOrdersAllFive)
    allOrders should equal(allOrdersExceptTheFilled1)
    activeOrdersBy(alicePair) should equal(activeOrdersByPair)
    allOrdersBy(alicePair) should equal(allOrdersByPair)

    info("'fullOrderHistory' and 'ordersByAddress' must return all active orders, even if they are more than the limit")

    val active3 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active4 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active5 = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 2.zbs, matcherFee).message.id
    val active6 = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 2.zbs, matcherFee).message.id

    matcher.waitOrderStatus(bobPair, active6, "Accepted", 1.minutes)

    val activeOrdersAllNine          = Seq(active6, active5, active4, active3) ++ activeOrdersAllFive
    val activeOrdersByPairWithTwoNew = Seq(active4, active3) ++ activeOrdersByPair
    val allOrdersByPairWithTwoNew    = Seq(active4, active3) ++ allOrdersByPair

    activeOrders should (equal(allOrders) and equal(activeOrdersAllNine))
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoNew)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoNew)

    info("'orderHistoryByPair' must return no more 'rest-order-limit' orders")

    val active7  = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 9.zbs, matcherFee).message.id
    val active8  = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active9  = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 1.zbs, matcherFee).message.id
    val active10 = matcher.placeOrder(aliceAcc, bobPair, BUY, 1, 1.zbs, matcherFee).message.id

    matcher.waitOrderStatus(bobPair, active10, "Accepted", 1.minutes)

    val activeOrdersAllThirteen               = Seq(active10, active9, active8, active7) ++ activeOrdersAllNine
    val activeOrdersByPairWithTwoMoreNew      = Seq(active8, active7) ++ activeOrdersByPairWithTwoNew
    val allOrdersByPairWithTwoNewExceptOneOld = Seq(active8, active7) ++ allOrdersByPairWithTwoNew.dropRight(1)

    activeOrders should (equal(allOrders) and equal(activeOrdersAllThirteen))
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoMoreNew)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoNewExceptOneOld)

    info("all the methods move active orders that were filled")

    matcher.placeOrder(bobAcc, bobPair, SELL, 1, 3.zbs, matcherFee).message.id   // fill partial2
    matcher.placeOrder(bobAcc, bobPair, SELL, 2, 2.zbs, matcherFee).message.id   // fill active2, active5
    matcher.placeOrder(bobAcc, alicePair, BUY, 2, 9.zbs, matcherFee).message.id  // fill partial1, active7
    matcher.placeOrder(bobAcc, alicePair, BUY, 1, 10.zbs, matcherFee).message.id // fill active1

    matcher.waitOrderStatus(bobPair, active1, "Filled", 1.minutes)

    val activeOrdersAllSeven            = Seq(active10, active9, active8, active6, active4, active3, active0)
    val allOrdersWithOneFilled          = activeOrdersAllSeven ++ Seq(active1)
    val activeOrdersByPairWithTwoFilled = Seq(active8, active4, active3, active0)
    val allOrdersByPairWithTwoFilled    = activeOrdersByPairWithTwoFilled ++ Seq(active7, cancelled1, partial1, active1)

    activeOrders should equal(activeOrdersAllSeven)
    allOrders should equal(allOrdersWithOneFilled)
    activeOrdersBy(alicePair) should equal(activeOrdersByPairWithTwoFilled)
    allOrdersBy(alicePair) should equal(allOrdersByPairWithTwoFilled)

    info("'orderHistoryByPair' must return all active orders, even if they are more than the limit")

    val active11 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active12 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active13 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active14 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id
    val active15 = matcher.placeOrder(aliceAcc, alicePair, SELL, 1, 10.zbs, matcherFee).message.id

    matcher.waitOrderStatus(bobPair, active15, "Accepted", 1.minutes)

    val activeOrdersAllTwelve     = Seq(active15, active14, active13, active12, active11) ++ activeOrdersAllSeven
    val activeOrdersByPairAllNine = Seq(active15, active14, active13, active12, active11) ++ allOrdersByPairWithTwoFilled.take(4)

    activeOrders should (equal(allOrders) and equal(activeOrdersAllTwelve))
    activeOrdersBy(alicePair) should (equal(allOrdersBy(alicePair)) and equal(activeOrdersByPairAllNine))
  }

}

object RestOrderLimitTestSuite {
  val reducedLimit                            = 8
  val configWithReducedRestOrderLimit: Config = parseString(s"""
                                                       |zbs.matcher {
                                                       |  rest-order-limit=$reducedLimit
                                                       |}
    """.stripMargin)

  val CustomConfigs: Seq[Config] = Configs.map(configWithReducedRestOrderLimit.withFallback(_))
}
