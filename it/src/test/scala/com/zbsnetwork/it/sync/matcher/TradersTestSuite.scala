package com.zbsplatform.it.sync.matcher

import com.typesafe.config.Config
import com.zbsplatform.it.ReportingTestName
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.api.SyncMatcherHttpApi._
import com.zbsplatform.it.sync._
import com.zbsplatform.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.util._
import com.zbsplatform.matcher.market.MatcherActor
import com.zbsplatform.matcher.model.MatcherModel.Price
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalatest.{BeforeAndAfterAll, CancelAfterFailure, FreeSpec, Matchers}

class TradersTestSuite extends FreeSpec with Matchers with BeforeAndAfterAll with CancelAfterFailure with NodesFromDocker with ReportingTestName {

  override protected def nodeConfigs: Seq[Config] = Configs

  private def matcherNode    = nodes.head
  private def aliceNode      = nodes(1)
  private def bobNode        = nodes(2)
  private val TransactionFee = 300000

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceNode.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(aliceAsset)

    // val aliceZbsPair = AssetPair(ByteStr.decodeBase58(aliceAsset).toOption, None)

    // Wait for balance on Alice's account
    aliceNode.assertAssetBalance(aliceNode.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherNode.address, aliceAsset, 0)
    bobNode.assertAssetBalance(bobNode.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobNode.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, false, 100000000L).id
    nodes.waitForHeightAriseAndTxPresent(bobNewAsset)
    val bobAssetId   = ByteStr.decodeBase58(bobNewAsset).get
    val aliceAssetId = ByteStr.decodeBase58(aliceAsset).get

    val bobZbsPair = AssetPair(
      amountAsset = Some(bobAssetId),
      priceAsset = None
    )

    val twoAssetsPair =
      if (MatcherActor.compare(Some(bobAssetId.arr), Some(aliceAssetId.arr)) < 0)
        AssetPair(
          amountAsset = Some(aliceAssetId),
          priceAsset = Some(bobAssetId)
        )
      else
        AssetPair(
          amountAsset = Some(bobAssetId),
          priceAsset = Some(aliceAssetId)
        )

    nodes.waitForHeightArise()
    bobNode.assertAssetBalance(bobNode.address, bobNewAsset, bobAssetQuantity)

    "matcher should respond with Public key" in {
      matcherNode.matcherGet("/matcher").getResponseBody.stripPrefix("\"").stripSuffix("\"") shouldBe matcherNode.publicKeyStr
    }

    "owner moves assets/zbs to another account and order become an invalid" ignore {
      // todo: reactivate after balance watcher is reimplemented
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(8000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobNode.address, aliceNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")

          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, 3050, TransactionFee, Some(bobNewAsset), None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

        "leased zbs, insufficient fee" in {
          val bobBalance    = bobNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - TransactionFee - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved zbs, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobNode.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - TransactionFee - matcherFee
          val transferId     = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferId)

          withClue(s"The oldest order '$oldestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, twoAssetsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }
      }

      "order with zbs" - {
        "leased zbs, insufficient fee for one ExchangeTransaction" in {
          // Amount of zbs in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1

          val oldestOrderId = bobPlacesZbsOrder(bobZbsPair, 10.zbs * Order.PriceConstant, 1)
          val newestOrderId = bobPlacesZbsOrder(bobZbsPair, 10.zbs * Order.PriceConstant, 1)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - TransactionFee - 10.zbs - matcherFee
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The newest order '$oldestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, oldestOrderId, "Cancelled")
          }
          withClue(s"The newest order '$newestOrderId' is still active") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Accepted")
          }

          // Cleanup
          nodes.waitForHeightArise()
          matcherNode.cancelOrder(bobNode, bobZbsPair, Some(newestOrderId)).status should be("OrderCanceled")
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "leased zbs, insufficient zbs" in {
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = 1.zbs
          val order2     = bobPlacesZbsOrder(bobZbsPair, price * Order.PriceConstant, 1)

          val leaseAmount = bobBalance - TransactionFee - price / 2
          val leaseId     = bobNode.lease(bobNode.address, aliceNode.address, leaseAmount, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, order2, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val cancelLeaseId = bobNode.cancelLease(bobNode.address, leaseId, TransactionFee).id
          nodes.waitForHeightAriseAndTxPresent(cancelLeaseId)
        }

        "moved zbs, insufficient fee" in {
          // Amount of zbs in order is smaller than fee
          val bobBalance = bobNode.accountBalances(bobNode.address)._1
          val price      = TransactionFee / 2
          val order3     = bobPlacesZbsOrder(bobZbsPair, price * Order.PriceConstant, 1)

          val transferAmount = bobBalance - TransactionFee - price
          val txId           = bobNode.transfer(bobNode.address, aliceNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, order3, "Cancelled")
          }

          // Cleanup
          nodes.waitForHeightArise()
          val transferBackId = aliceNode.transfer(aliceNode.address, bobNode.address, transferAmount, TransactionFee, None, None).id
          nodes.waitForHeightAriseAndTxPresent(transferBackId)
        }

      }
    }
  }

  def bobPlacesZbsOrder(assetPair: AssetPair, price: Price, amount: Long): String = {
    val bobOrder = matcherNode.prepareOrder(bobNode, assetPair, OrderType.BUY, price, amount)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.SELL, 1 * Order.PriceConstant, bobCoinAmount)
    } else {
      matcherNode.prepareOrder(bobNode, twoAssetsPair, OrderType.BUY, bobCoinAmount * Order.PriceConstant, 1)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
