package com.zbsplatform.it.sync.matcher

import com.typesafe.config.Config
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.LevelResponse
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.sync._
import com.zbsplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.assets.exchange.{AssetPair, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._

class RoundingIssuesTestSuite
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

  Seq(IssueUsdTx, IssueEthTx, IssueBtcTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  "should correctly fill an order with small amount" in {
    val aliceBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val counter   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, 238, 3100000000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, 235, 425532L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 420169L
    matcherNode.waitOrderStatusAndAmount(zbsUsdPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(zbsUsdPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    matcherNode.cancelOrder(aliceNode, zbsUsdPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)
    val rawExchangeTx = matcherNode.rawTransactionInfo(tx.id)

    (rawExchangeTx \ "price").as[Long] shouldBe counter.price
    (rawExchangeTx \ "amount").as[Long] shouldBe filledAmount
    (rawExchangeTx \ "buyMatcherFee").as[Long] shouldBe 40L
    (rawExchangeTx \ "sellMatcherFee").as[Long] shouldBe 296219L

    val aliceBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
    val bobBalanceAfter   = matcherNode.accountBalances(bobNode.address)._1

    (aliceBalanceAfter - aliceBalanceBefore) shouldBe (-40L + 420169L)
    (bobBalanceAfter - bobBalanceBefore) shouldBe (-296219L - 420169L)
  }

  "reserved balance should not be negative" in {
    val counter   = matcherNode.prepareOrder(aliceNode, ethBtcPair, OrderType.BUY, 31887L, 923431000L)
    val counterId = matcherNode.placeOrder(counter).message.id

    val submitted   = matcherNode.prepareOrder(bobNode, ethBtcPair, OrderType.SELL, 31887L, 223345000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    val filledAmount = 223344937L
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, submittedId, "Filled", Some(filledAmount), 1.minute)
    matcherNode.waitOrderStatusAndAmount(ethBtcPair, counterId, "PartiallyFilled", Some(filledAmount), 1.minute)

    withClue("Bob's reserved balance before cancel")(matcherNode.reservedBalance(bobNode) shouldBe empty)

    matcherNode.cancelOrder(aliceNode, ethBtcPair, Some(counterId))
    val tx = matcherNode.transactionsByOrder(counterId).head

    matcherNode.waitForTransaction(tx.id)

    withClue("Alice's reserved balance after cancel")(matcherNode.reservedBalance(aliceNode) shouldBe empty)
  }

  "should correctly fill 2 counter orders" in {
    val counter1 = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, 60L, 98333333L)
    matcherNode.placeOrder(counter1).message.id

    val counter2   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, 70L, 100000000L)
    val counter2Id = matcherNode.placeOrder(counter2).message.id

    val submitted   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, 1000L, 100000000L)
    val submittedId = matcherNode.placeOrder(submitted).message.id

    matcherNode.waitOrderStatusAndAmount(zbsUsdPair, counter2Id, "PartiallyFilled", Some(2857143L), 1.minute)
    matcherNode.waitOrderStatusAndAmount(zbsUsdPair, submittedId, "Filled", Some(99523810L), 1.minute)

    withClue("orderBook check") {
      val ob = matcherNode.orderBook(zbsUsdPair)
      ob.bids shouldBe empty
      ob.asks shouldBe List(LevelResponse(70L, 97142857L)) // = 100000000 - 2857143
    }
  }

}
