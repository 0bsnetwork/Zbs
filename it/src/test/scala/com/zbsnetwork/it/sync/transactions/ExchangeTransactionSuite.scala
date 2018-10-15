package com.zbsplatform.it.sync.transactions

import com.zbsplatform.api.http.assets.SignedExchangeRequest
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.sync._
import com.zbsplatform.it.transactions.BaseTransactionSuite
import com.zbsplatform.it.util._
import com.zbsplatform.transaction.assets.IssueTransactionV1
import com.zbsplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.zbsplatform.utils.{Base58, NTP}
import play.api.libs.json._

class ExchangeTransactionSuite extends BaseTransactionSuite {

  test("cannot exchange non-issued assets") {

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
    val seller              = pkByAddress(firstAddress)
    val matcher             = pkByAddress(thirdAddress)
    val time                = NTP.correctedTime()
    val expirationTimestamp = time + Order.MaxLiveTime
    val buyPrice            = 2 * Order.PriceConstant
    val sellPrice           = 2 * Order.PriceConstant
    val buyAmount           = 1
    val sellAmount          = 1
    val assetPair           = AssetPair.createAssetPair("ZBS", assetId).get
    val buy                 = Order.buy(buyer, matcher, assetPair, buyPrice, buyAmount, time, expirationTimestamp, matcherFee)
    val sell                = Order.sell(seller, matcher, assetPair, sellPrice, sellAmount, time, expirationTimestamp, matcherFee)

    val amount = 1
    val tx = ExchangeTransaction
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        price = sellPrice,
        amount = amount,
        buyMatcherFee = (BigInt(matcherFee) * amount / buy.amount).toLong,
        sellMatcherFee = (BigInt(matcherFee) * amount / sell.amount).toLong,
        fee = matcherFee,
        timestamp = NTP.correctedTime()
      )
      .right
      .get

    implicit val o: Writes[Order] = Writes[Order](_.json())

    implicit val w: Writes[SignedExchangeRequest] =
      Json.writes[SignedExchangeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(ExchangeTransaction.typeId.toInt)))

    def request(tx: ExchangeTransaction): SignedExchangeRequest =
      SignedExchangeRequest(
        Base58.encode(tx.sender.publicKey),
        tx.buyOrder,
        tx.sellOrder,
        tx.price,
        tx.amount,
        matcherFee,
        tx.buyMatcherFee,
        tx.sellMatcherFee,
        tx.timestamp,
        tx.signature.base58
      )

    assertBadRequestAndMessage(sender.postJson("/transactions/broadcast", request(tx)), "Assets should be issued before they can be traded")
  }

}
