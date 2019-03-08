package com.zbsnetwork.http

import com.zbsnetwork.api.http.{ApiKeyNotValid, PaymentApiRoute}
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.http.ApiMarshallers._
import com.zbsnetwork.state.Diff
import com.zbsnetwork.transaction.Transaction
import com.zbsnetwork.transaction.transfer._
import com.zbsnetwork.utils.Time
import com.zbsnetwork.utx.UtxPool
import com.zbsnetwork.{NoShrink, TestWallet, TransactionGen}
import io.netty.channel.group.{ChannelGroup, ChannelGroupFuture, ChannelMatcher}
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, Json}

class PaymentRouteSpec
    extends RouteSpec("/payment")
    with MockFactory
    with PropertyChecks
    with RestAPISettingsHelper
    with TestWallet
    with TransactionGen
    with NoShrink {

  private val utx         = stub[UtxPool]
  private val allChannels = stub[ChannelGroup]

  (utx.putIfNew _).when(*).onCall((t: Transaction) => Right((true, Diff.empty))).anyNumberOfTimes()
  (allChannels.writeAndFlush(_: Any, _: ChannelMatcher)).when(*, *).onCall((_: Any, _: ChannelMatcher) => stub[ChannelGroupFuture]).anyNumberOfTimes()

  "accepts payments" in {
    forAll(accountOrAliasGen.label("recipient"), positiveLongGen.label("amount"), smallFeeGen.label("fee")) {
      case (recipient, amount, fee) =>
        val timestamp = System.currentTimeMillis()

        val time = new Time {
          override def correctedTime(): Long = timestamp

          override def getTimestamp(): Long = timestamp
        }

        val sender = testWallet.privateKeyAccounts.head
        val tx     = TransferTransactionV1.selfSigned(None, sender, recipient, amount, timestamp, None, fee, Array())

        val route = PaymentApiRoute(restAPISettings, testWallet, utx, allChannels, time).route

        val req = Json.obj("sender" -> sender.address, "recipient" -> recipient.stringRepr, "amount" -> amount, "fee" -> fee)

        Post(routePath(""), req) ~> route should produce(ApiKeyNotValid)
        Post(routePath(""), req) ~> api_key(apiKey) ~> route ~> check {
          val resp = responseAs[JsObject]

          (resp \ "id").as[String] shouldEqual tx.explicitGet().id().toString
          (resp \ "assetId").asOpt[String] shouldEqual None
          (resp \ "feeAsset").asOpt[String] shouldEqual None
          (resp \ "type").as[Int] shouldEqual 4
          (resp \ "fee").as[Int] shouldEqual fee
          (resp \ "amount").as[Long] shouldEqual amount
          (resp \ "timestamp").as[Long] shouldEqual tx.explicitGet().timestamp
          (resp \ "sender").as[String] shouldEqual sender.address
          (resp \ "recipient").as[String] shouldEqual recipient.stringRepr
        }
    }
  }
}
