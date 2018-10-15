package com.zbsplatform.it.sync.matcher

import com.typesafe.config.Config
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.AssetDecimalsInfo
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.sync.CustomFeeTransactionSuite.defaultAssetQuantity
import com.zbsplatform.it.sync._
import com.zbsplatform.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.util._
import com.zbsplatform.matcher.model.LimitOrder
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

import scala.concurrent.duration._
import scala.math.BigDecimal.RoundingMode

class TradeBalanceAndRoundingTestSuite
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

  Seq(IssueUsdTx, IssueEthTx, IssueWctTx).map(createSignedIssueRequest).foreach(matcherNode.signedIssue)
  nodes.waitForHeightArise()

  "Alice and Bob trade ZBS-USD" - {
    nodes.waitForHeightArise()
    val aliceZbsBalanceBefore = matcherNode.accountBalances(aliceNode.address)._1
    val bobZbsBalanceBefore   = matcherNode.accountBalances(bobNode.address)._1

    val price           = 238
    val buyOrderAmount  = 425532L
    val sellOrderAmount = 3100000000L

    val correctedSellAmount = correctAmount(sellOrderAmount, price)

    val adjustedAmount = receiveAmount(OrderType.BUY, price, buyOrderAmount)
    val adjustedTotal  = receiveAmount(OrderType.SELL, price, buyOrderAmount)

    log.debug(s"correctedSellAmount: $correctedSellAmount, adjustedAmount: $adjustedAmount, adjustedTotal: $adjustedTotal")

    "place usd-zbs order" in {
      // Alice wants to sell USD for Zbs

      val bobOrder1   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price, sellOrderAmount)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Accepted", 1.minute)
      matcherNode.reservedBalance(bobNode)("ZBS") shouldBe sellOrderAmount + matcherFee
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS") shouldBe bobZbsBalanceBefore - (sellOrderAmount + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price, buyOrderAmount)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatusAndAmount(zbsUsdPair, aliceOrderId, "Filled", Some(420169L), 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatusAndAmount(zbsUsdPair, bobOrder1Id, "PartiallyFilled", Some(420169L), 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.id().base58).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
    }

    "get opened trading markets. USD price-asset " in {
      val openMarkets = matcherNode.tradingMarkets()
      openMarkets.markets.size shouldBe 1
      val markets = openMarkets.markets.head

      markets.amountAssetName shouldBe "ZBS"
      markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(8))

      markets.priceAssetName shouldBe usdAssetName
      markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
    }

    "check usd and zbs balance after fill" in {
      val aliceZbsBalanceAfter = matcherNode.accountBalances(aliceNode.address)._1
      val aliceUsdBalance        = matcherNode.assetBalance(aliceNode.address, UsdId.base58).balance

      val bobZbsBalanceAfter = matcherNode.accountBalances(bobNode.address)._1
      val bobUsdBalance        = matcherNode.assetBalance(bobNode.address, UsdId.base58).balance

      (aliceZbsBalanceAfter - aliceZbsBalanceBefore) should be(
        adjustedAmount - (BigInt(matcherFee) * adjustedAmount / buyOrderAmount).bigInteger.longValue())

      aliceUsdBalance - defaultAssetQuantity should be(-adjustedTotal)
      bobZbsBalanceAfter - bobZbsBalanceBefore should be(
        -adjustedAmount - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount).bigInteger.longValue())
      bobUsdBalance should be(adjustedTotal)
    }

    "check filled amount and tradable balance" in {
      val bobsOrderId  = matcherNode.fullOrderHistory(bobNode).head.id
      val filledAmount = matcherNode.orderStatus(bobsOrderId, zbsUsdPair).filledAmount.getOrElse(0L)

      filledAmount shouldBe adjustedAmount
    }

    "check reserved balance" in {
      val reservedFee = BigInt(matcherFee) - (BigInt(matcherFee) * adjustedAmount / sellOrderAmount)
      log.debug(s"reservedFee: $reservedFee")
      val expectedBobReservedBalance = correctedSellAmount - adjustedAmount + reservedFee
      matcherNode.reservedBalance(bobNode)("ZBS") shouldBe expectedBobReservedBalance

      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    "check zbs-usd tradable balance" in {
      val expectedBobTradableBalance = bobZbsBalanceBefore - (correctedSellAmount + matcherFee)
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS") shouldBe expectedBobTradableBalance
      matcherNode.tradableBalance(aliceNode, zbsUsdPair)("ZBS") shouldBe aliceNode.accountBalances(aliceNode.address)._1

      val orderId = matcherNode.fullOrderHistory(bobNode).head.id
      matcherNode.fullOrderHistory(bobNode).size should be(1)
      matcherNode.cancelOrder(bobNode, zbsUsdPair, Some(orderId))
      matcherNode.waitOrderStatus(zbsUsdPair, orderId, "Cancelled", 1.minute)
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS") shouldBe bobNode.accountBalances(bobNode.address)._1
    }
  }

  "Alice and Bob trade ZBS-USD check CELLING" - {
    val price2           = 289
    val buyOrderAmount2  = 0.07.zbs
    val sellOrderAmount2 = 3.zbs

    val correctedSellAmount2 = correctAmount(sellOrderAmount2, price2)

    "place usd-zbs order" in {
      nodes.waitForHeightArise()
      // Alice wants to sell USD for Zbs
      val bobZbsBalanceBefore = matcherNode.accountBalances(bobNode.address)._1
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS")
      val bobOrder1   = matcherNode.prepareOrder(bobNode, zbsUsdPair, OrderType.SELL, price2, sellOrderAmount2)
      val bobOrder1Id = matcherNode.placeOrder(bobOrder1).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "Accepted", 1.minute)

      matcherNode.reservedBalance(bobNode)("ZBS") shouldBe correctedSellAmount2 + matcherFee
      matcherNode.tradableBalance(bobNode, zbsUsdPair)("ZBS") shouldBe bobZbsBalanceBefore - (correctedSellAmount2 + matcherFee)

      val aliceOrder   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, price2, buyOrderAmount2)
      val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id
      matcherNode.waitOrderStatus(zbsUsdPair, aliceOrderId, "Filled", 1.minute)

      // Bob wants to buy some USD
      matcherNode.waitOrderStatus(zbsUsdPair, bobOrder1Id, "PartiallyFilled", 1.minute)

      // Each side get fair amount of assets
      val exchangeTx = matcherNode.transactionsByOrder(aliceOrder.id().base58).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.cancelOrder(bobNode, zbsUsdPair, Some(bobOrder1Id))
    }

  }

  "Alice and Bob trade WCT-USD sell price less than buy price" - {
    "place wcd-usd order corrected by new price sell amount less then initial one" in {
      val buyPrice   = 247700
      val sellPrice  = 135600
      val buyAmount  = 46978
      val sellAmount = 56978

      val bobOrderId = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, sellPrice, sellAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)
      val aliceOrderId = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, buyPrice, buyAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      val exchangeTx = matcherNode.transactionsByOrder(aliceOrderId).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)
      matcherNode.cancelOrder(bobNode, wctUsdPair, Some(bobOrderId))

      matcherNode.waitOrderStatus(wctUsdPair, bobOrderId, "Cancelled", 1.minute)

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }
  }

  "Alice and Bob trade WCT-USD 1" - {
    val wctUsdSellAmount = 347
    val wctUsdBuyAmount  = 146
    val wctUsdPrice      = 12739213

    "place wct-usd order" in {
      nodes.waitForSameBlockHeadesAt(nodes.map(_.height).max + 1)

      val aliceUsdBalance   = matcherNode.assetBalance(aliceNode.address, UsdId.base58).balance
      val bobUsdBalance     = matcherNode.assetBalance(bobNode.address, UsdId.base58).balance
      val bobWctInitBalance = matcherNode.assetBalance(bobNode.address, WctId.base58).balance

      val bobOrderId = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, wctUsdPrice, wctUsdSellAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrderId, "Accepted", 1.minute)

      val aliceOrderId = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, wctUsdPrice, wctUsdBuyAmount).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      val exchangeTx = matcherNode.transactionsByOrder(aliceOrderId).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)

      val executedAmount         = correctAmount(wctUsdBuyAmount, wctUsdPrice) // 142
      val bobReceiveUsdAmount    = receiveAmount(SELL, wctUsdBuyAmount, wctUsdPrice)
      val expectedReservedBobWct = wctUsdSellAmount - executedAmount // 205 = 347 - 142

      matcherNode.reservedBalance(bobNode)(s"$WctId") shouldBe expectedReservedBobWct
      // 999999999652 = 999999999999 - 142 - 205
      matcherNode.tradableBalance(bobNode, wctUsdPair)(s"$WctId") shouldBe bobWctInitBalance - executedAmount - expectedReservedBobWct
      matcherNode.tradableBalance(bobNode, wctUsdPair)(s"$UsdId") shouldBe bobUsdBalance + bobReceiveUsdAmount

      matcherNode.reservedBalance(aliceNode) shouldBe empty
      matcherNode.tradableBalance(aliceNode, wctUsdPair)(s"$UsdId") shouldBe aliceUsdBalance - bobReceiveUsdAmount

      val expectedReservedZbs = matcherFee - LimitOrder.getPartialFee(matcherFee, wctUsdSellAmount, executedAmount)
      matcherNode.reservedBalance(bobNode)("ZBS") shouldBe expectedReservedZbs

      matcherNode.cancelOrder(bobNode, wctUsdPair, Some(matcherNode.fullOrderHistory(bobNode).head.id))
    }

    "reserved balance is empty after the total execution" in {
      val aliceOrderId = matcherNode.placeOrder(aliceNode, wctUsdPair, BUY, 100000, 5000000).message.id
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrderId, "Accepted", 1.minute)

      val bobOrderId = matcherNode.placeOrder(bobNode, wctUsdPair, SELL, 99908, 5000000).message.id
      matcherNode.waitOrderStatus(wctUsdPair, bobOrderId, "Filled", 1.minute)
      matcherNode.waitOrderStatus(wctUsdPair, aliceOrderId, "Filled", 1.minute)

      val exchangeTx = matcherNode.transactionsByOrder(bobOrderId).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)

      matcherNode.reservedBalance(aliceNode) shouldBe empty
      matcherNode.reservedBalance(bobNode) shouldBe empty
    }

  }

  "get opened trading markets. Check WCT-USD" in {
    val openMarkets = matcherNode.tradingMarkets()
    val markets     = openMarkets.markets.last

    markets.amountAssetName shouldBe wctAssetName
    markets.amountAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))

    markets.priceAssetName shouldBe usdAssetName
    markets.priceAssetInfo shouldBe Some(AssetDecimalsInfo(Decimals))
  }

  "Alice and Bob trade WCT-ZBS on not enough fee when place order" - {
    val wctZbsSellAmount = 2
    val wctZbsPrice      = 11234560000000L

    "bob lease all zbs exact half matcher fee" in {
      val leasingAmount = bobNode.accountBalances(bobNode.address)._1 - leasingFee - matcherFee / 2
      val leaseTxId     = bobNode.lease(bobNode.address, matcherNode.address, leasingAmount, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(leaseTxId)
      val bobOrderId = matcherNode.placeOrder(bobNode, wctZbsPair, SELL, wctZbsPrice, wctZbsSellAmount).message.id
      matcherNode.waitOrderStatus(wctZbsPair, bobOrderId, "Accepted", 1.minute)

      matcherNode.tradableBalance(bobNode, wctZbsPair)("ZBS") shouldBe matcherFee / 2 + receiveAmount(SELL, wctZbsPrice, wctZbsSellAmount) - matcherFee
      matcherNode.cancelOrder(bobNode, wctZbsPair, Some(bobOrderId))

      assertBadRequestAndResponse(matcherNode.placeOrder(bobNode, wctZbsPair, SELL, wctZbsPrice, wctZbsSellAmount / 2),
                                  "Not enough tradable balance")

      val cancelLeaseTxId = bobNode.cancelLease(bobNode.address, leaseTxId, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(cancelLeaseTxId)
    }
  }

  "Alice and Bob trade ETH-ZBS" - {
    "reserved balance is empty after the total execution" in {
      val counterId1 = matcherNode.placeOrder(aliceNode, ethZbsPair, SELL, 300000, 2864310).message.id
      matcherNode.waitOrderStatus(ethZbsPair, counterId1, "Accepted", 1.minute)

      val counterId2 = matcherNode.placeOrder(aliceNode, ethZbsPair, SELL, 300000, 7237977).message.id
      matcherNode.waitOrderStatus(ethZbsPair, counterId2, "Accepted", 1.minute)

      val submittedId = matcherNode.placeOrder(bobNode, ethZbsPair, BUY, 300000, 4373667).message.id

      matcherNode.waitOrderStatus(ethZbsPair, counterId1, "Filled", 1.minute)
      matcherNode.waitOrderStatus(ethZbsPair, counterId2, "PartiallyFilled", 1.minute)
      matcherNode.waitOrderStatus(ethZbsPair, submittedId, "Filled", 1.minute)

      val exchangeTx = matcherNode.transactionsByOrder(submittedId).headOption.getOrElse(fail("Expected an exchange transaction"))
      nodes.waitForHeightAriseAndTxPresent(exchangeTx.id)

      matcherNode.reservedBalance(bobNode) shouldBe empty
      matcherNode.cancelOrder(aliceNode, ethZbsPair, Some(counterId2))
    }
  }

  "Submitted order Canceled during match" in {
    val bobOrder   = matcherNode.prepareOrder(matcherNode, zbsUsdPair, OrderType.SELL, 10L, 10000000L)
    val bobOrderId = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(zbsUsdPair, bobOrderId, "Accepted", 1.minute)

    val aliceOrder   = matcherNode.prepareOrder(aliceNode, zbsUsdPair, OrderType.BUY, 1000L, 100000L)
    val aliceOrderId = matcherNode.placeOrder(aliceOrder).message.id

    matcherNode.waitOrderStatusAndAmount(zbsUsdPair, aliceOrderId, "Filled", Some(0), 1.minute)

    withClue("Alice's reserved balance:") {
      matcherNode.reservedBalance(aliceNode) shouldBe empty
    }

    val aliceOrders = matcherNode.ordersByAddress(aliceNode.address, activeOnly = false, 1.minute)
    aliceOrders should not be empty

    val order = aliceOrders.find(_.id == aliceOrderId).getOrElse(throw new IllegalStateException(s"Alice should have the $aliceOrderId order"))
    order.status shouldBe "Filled"

    matcherNode.cancelOrder(matcherNode, zbsUsdPair, Some(bobOrderId))
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
