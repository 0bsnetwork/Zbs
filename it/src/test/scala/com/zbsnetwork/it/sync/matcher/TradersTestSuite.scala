package com.zbsnetwork.it.sync.matcher

import com.typesafe.config.Config
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.matcher.MatcherSuiteBase
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.sync.matcher.config.MatcherPriceAssetConfig._
import com.zbsnetwork.it.util._
import com.zbsnetwork.matcher.market.MatcherActor
import com.zbsnetwork.matcher.model.MatcherModel.Price
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order, OrderType}

import scala.util.Random

class TradersTestSuite extends MatcherSuiteBase {
  private val exTxFee                             = 300000
  private def orderVersion                        = (Random.nextInt(2) + 1).toByte
  override protected def nodeConfigs: Seq[Config] = Configs

  "Verifications of tricky ordering cases" - {
    // Alice issues new asset
    val aliceAsset =
      aliceNode.issue(aliceAcc.address, "AliceCoin", "AliceCoin for matcher's tests", someAssetAmount, 0, reissuable = false, smartIssueFee, 2).id
    matcherNode.waitForTransaction(aliceAsset)

    // Wait for balance on Alice's account
    matcherNode.assertAssetBalance(aliceAcc.address, aliceAsset, someAssetAmount)
    matcherNode.assertAssetBalance(matcherAcc.address, aliceAsset, 0)
    matcherNode.assertAssetBalance(bobAcc.address, aliceAsset, 0)

    // Bob issues a new asset
    val bobAssetQuantity = 10000
    val bobNewAsset      = bobNode.issue(bobAcc.address, "BobCoin3", "Bob's asset", bobAssetQuantity, 0, reissuable = false, smartIssueFee, 2).id
    matcherNode.waitForTransaction(bobNewAsset)

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

    matcherNode.assertAssetBalance(bobAcc.address, bobNewAsset, bobAssetQuantity)

    "owner moves assets/zbs to another account and order become an invalid" - {
      // Could not work sometimes because of NODE-546
      "order with assets" - {
        "moved assets, insufficient assets" in {
          val oldestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(4000, twoAssetsPair, bobNewAsset)

          val transferId = bobNode.transfer(bobAcc.address, aliceAcc.address, 5000, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferId) // 5000 zbs are rest

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobZbsPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, 5000, exTxFee, Some(bobNewAsset), None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

        "leased zbs, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val leaseAmount = bobBalance - exTxFee - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobZbsPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved zbs, insufficient fee" in {
          val bobBalance    = matcherNode.accountBalances(bobAcc.address)._1
          val oldestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)
          val newestOrderId = bobPlacesAssetOrder(1000, twoAssetsPair, bobNewAsset)

          // TransactionFee for leasing, matcherFee for one order
          val transferAmount = bobBalance - exTxFee - matcherFee
          val transferId     = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferId)

          withClue(s"The newest order '$newestOrderId' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobZbsPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, twoAssetsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }
      }

      "order with zbs" - {
        "leased zbs, insufficient fee for one ExchangeTransaction" in {
          // Amount of zbs in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1

          val oldestOrderId = bobPlacesWaveOrder(bobZbsPair, 1, 10.zbs * Order.PriceConstant)
          val newestOrderId = bobPlacesWaveOrder(bobZbsPair, 1, 10.zbs * Order.PriceConstant)

          //      waitForOrderStatus(matcherNode, bobAssetIdRaw, id, "Accepted")
          val leaseAmount = bobBalance - exTxFee - 10.zbs - matcherFee
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The newest order '$newestOrderId' is Cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, newestOrderId, "Cancelled")
          }
          withClue(s"The oldest order '$oldestOrderId' is still active") {
            matcherNode.orderStatus(oldestOrderId, bobZbsPair).status shouldBe "Accepted"
          }

          // Cleanup
          matcherNode.cancelOrder(bobAcc, bobZbsPair, oldestOrderId)
          matcherNode.waitOrderStatus(twoAssetsPair, oldestOrderId, "Cancelled")

          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "leased zbs, insufficient zbs" in {
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = 1.zbs
          val order2     = bobPlacesWaveOrder(bobZbsPair, 1, price * Order.PriceConstant)

          val leaseAmount = bobBalance - exTxFee - price / 2
          val leaseId     = bobNode.lease(bobAcc.address, aliceAcc.address, leaseAmount, exTxFee, 2).id
          matcherNode.waitForTransaction(leaseId)

          withClue(s"The order '$order2' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, order2, "Cancelled")
          }

          // Cleanup
          val cancelLeaseId = bobNode.cancelLease(bobAcc.address, leaseId, exTxFee, 2).id
          matcherNode.waitForTransaction(cancelLeaseId)
        }

        "moved zbs, insufficient fee" in {
          // Amount of zbs in order is smaller than fee
          val bobBalance = matcherNode.accountBalances(bobAcc.address)._1
          val price      = exTxFee / 2
          val order3     = bobPlacesWaveOrder(bobZbsPair, 1, price * Order.PriceConstant)

          val transferAmount = bobBalance - exTxFee - price
          val txId           = bobNode.transfer(bobAcc.address, aliceAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(txId)

          withClue(s"The order '$order3' was cancelled") {
            matcherNode.waitOrderStatus(bobZbsPair, order3, "Cancelled")
          }

          // Cleanup
          val transferBackId = aliceNode.transfer(aliceAcc.address, bobAcc.address, transferAmount, exTxFee, None, None, 2).id
          matcherNode.waitForTransaction(transferBackId)
        }

      }
    }
  }

  def bobPlacesWaveOrder(assetPair: AssetPair, amount: Long, price: Price): String = {
    val bobOrder = matcherNode.prepareOrder(bobAcc, assetPair, OrderType.BUY, amount, price)
    val order    = matcherNode.placeOrder(bobOrder).message.id
    matcherNode.waitOrderStatus(assetPair, order, "Accepted")
    order
  }

  def bobPlacesAssetOrder(bobCoinAmount: Int, twoAssetsPair: AssetPair, assetId: String): String = {
    val decodedAsset = ByteStr.decodeBase58(assetId).get
    val bobOrder = if (twoAssetsPair.amountAsset.contains(decodedAsset)) {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.SELL, bobCoinAmount, 1 * Order.PriceConstant, exTxFee, orderVersion)
    } else {
      matcherNode.prepareOrder(bobAcc, twoAssetsPair, OrderType.BUY, 1, bobCoinAmount * Order.PriceConstant, exTxFee, orderVersion)
    }
    val order = matcherNode.placeOrder(bobOrder)
    matcherNode.waitOrderStatus(twoAssetsPair, order.message.id, "Accepted")
    order.message.id
  }

}
