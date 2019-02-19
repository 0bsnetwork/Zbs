package com.zbsnetwork.matcher.model

import com.zbsnetwork.NoShrink
import com.zbsnetwork.matcher.MatcherTestData
import com.zbsnetwork.matcher.market.MatcherActor.OrderBookCreated
import com.zbsnetwork.matcher.model.EventSerializers._
import com.zbsnetwork.transaction.assets.exchange.AssetPair
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EventJsonSpecification extends PropSpec with PropertyChecks with Matchers with MatcherTestData with NoShrink {

  val buyLevelGen: Gen[Vector[BuyLimitOrder]] =
    Gen.containerOf[Vector, BuyLimitOrder](buyLimitOrderGenerator)

  val sellLevelGen: Gen[Vector[SellLimitOrder]] =
    Gen.containerOf[Vector, SellLimitOrder](sellLimitOrderGenerator)

  property("Write/Read OrderBook and Snapshot") {
    pending
  }

  property("OrderBookCreated json serialization roundtrip") {
    forAll(assetPairGen) { pair: AssetPair =>
      val obc = OrderBookCreated(pair)
      val js  = orderBookCreatedFormat.writes(obc)
      val r   = js.as[OrderBookCreated]
      obc shouldBe r
    }
  }
}
