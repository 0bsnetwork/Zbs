package com.zbsnetwork.it.sync.transactions

import com.zbsnetwork.it.NTPTime
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.assets.IssueTransactionV1
import com.zbsnetwork.transaction.assets.exchange._

class ExchangeTransactionSuite extends BaseTransactionSuite with NTPTime {
  test("cannot exchange non-issued assets") {
    for ((o1ver, o2ver, tver) <- Seq(
           (1: Byte, 1: Byte, 1: Byte),
           (1: Byte, 1: Byte, 2: Byte),
           (1: Byte, 2: Byte, 2: Byte),
           (2: Byte, 1: Byte, 2: Byte),
           (2: Byte, 2: Byte, 2: Byte)
         )) {
      val assetName        = "myasset"
      val assetDescription = "my asset description"

      val IssueTx: IssueTransactionV1 = IssueTransactionV1
        .selfSigned(
          sender = sender.privateKey,
          name = assetName.getBytes(),
          description = assetDescription.getBytes(),
          quantity = someAssetAmount,
          decimals = 2,
          reissuable = true,
          fee = 1.zbs,
          timestamp = System.currentTimeMillis()
        )
        .right
        .get

      val assetId = IssueTx.id().base58

      val buyer               = pkByAddress(firstAddress)
      val seller              = pkByAddress(secondAddress)
      val matcher             = pkByAddress(thirdAddress)
      val ts                  = ntpTime.correctedTime()
      val expirationTimestamp = ts + Order.MaxLiveTime
      val buyPrice            = 2 * Order.PriceConstant
      val sellPrice           = 2 * Order.PriceConstant
      val buyAmount           = 1
      val sellAmount          = 1
      val assetPair           = AssetPair.createAssetPair("ZBS", assetId).get
      val buy                 = Order.buy(buyer, matcher, assetPair, buyAmount, buyPrice, ts, expirationTimestamp, matcherFee, o1ver)
      val sell                = Order.sell(seller, matcher, assetPair, sellAmount, sellPrice, ts, expirationTimestamp, matcherFee, o2ver)

      val amount = 1
      if (tver != 1) {
        val tx = ExchangeTransactionV2
          .create(
            matcher = matcher,
            buyOrder = buy,
            sellOrder = sell,
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json()),
          "Assets should be issued before they can be traded"
        )
      } else {
        val tx = ExchangeTransactionV1
          .create(
            matcher = matcher,
            buyOrder = buy.asInstanceOf[OrderV1],
            sellOrder = sell.asInstanceOf[OrderV1],
            amount = amount,
            price = sellPrice,
            buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
            sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
            fee = matcherFee,
            timestamp = ntpTime.correctedTime()
          )
          .right
          .get

        assertBadRequestAndMessage(
          sender.postJson("/transactions/broadcast", tx.json()),
          "Assets should be issued before they can be traded"
        )
      }
    }

  }

}
