package com.zbsnetwork.matcher.model

import com.zbsnetwork.NoShrink
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.matcher.MatcherTestData
import com.zbsnetwork.state.Blockchain
import com.zbsnetwork.state.diffs.produce
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, ExchangeTransactionV1, ExchangeTransactionV2}
import org.scalamock.scalatest.PathMockFactory
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class ExchangeTransactionCreatorSpecification
    extends WordSpec
    with Matchers
    with MatcherTestData
    with BeforeAndAfterAll
    with PathMockFactory
    with PropertyChecks
    with NoShrink {

  private val pair = AssetPair(None, mkAssetId("BTC"))

  "ExchangeTransactionCreator" when {
    "SmartAccountTrading hasn't been activated yet" should {
      "create an ExchangeTransactionV1" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L))
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L))

        val bc = stub[Blockchain]
        (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV1]
      }

      "return an error" when {
        List((1, 2), (2, 1), (2, 2)).foreach {
          case (buyVersion, sellVersion) =>
            s"buyV$buyVersion and sellV$sellVersion" in {
              val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = buyVersion.toByte)
              val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = sellVersion.toByte)

              val bc = stub[Blockchain]
              (bc.activatedFeatures _).when().returns(Map.empty).anyNumberOfTimes()
              val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
              tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()) should produce(
                "SmartAccountTrading has not been activated yet")
            }
        }
      }
    }

    "SmartAccountTrading has been activated" should {
      "create an ExchangeTransactionV2" in {
        val counter   = buy(pair, 100000, 0.0008, matcherFee = Some(2000L), version = 2)
        val submitted = sell(pair, 100000, 0.0007, matcherFee = Some(1000L), version = 2)

        val bc = stub[Blockchain]
        (bc.activatedFeatures _).when().returns(Map(BlockchainFeatures.SmartAccountTrading.id -> 0)).anyNumberOfTimes()
        val tc = new ExchangeTransactionCreator(bc, MatcherAccount, matcherSettings)
        tc.createTransaction(LimitOrder(submitted), LimitOrder(counter), System.currentTimeMillis()).explicitGet() shouldBe a[ExchangeTransactionV2]
      }
    }
  }
}
