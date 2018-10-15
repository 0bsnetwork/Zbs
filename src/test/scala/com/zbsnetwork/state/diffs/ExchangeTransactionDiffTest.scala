package com.zbsplatform.state.diffs

import cats._
import com.zbsplatform.features.BlockchainFeatures
import com.zbsplatform.lang.v1.compiler.Terms.TRUE
import com.zbsplatform.settings.{Constants, TestFunctionalitySettings}
import com.zbsplatform.state._
import com.zbsplatform.state.diffs.TransactionDiffer.TransactionValidationError
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.ValidationError.AccountBalanceError
import com.zbsplatform.transaction.assets.{IssueTransaction, IssueTransactionV1}
import com.zbsplatform.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.{GenesisTransaction, ValidationError}

class ExchangeTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with Inside with NoShrink {

  property("preserves zbs invariant, stores match info, rewards matcher") {

    val preconditionsAndExchange: Gen[(GenesisTransaction, GenesisTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      maybeAsset1              <- Gen.option(issue1.id())
      maybeAsset2              <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
      exchange                 <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case (gen1, gen2, issue1, issue2, exchange) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2))), TestBlock.create(Seq(exchange))) {
          case (blockDiff, state) =>
            val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
            totalPortfolioDiff.balance shouldBe 0
            totalPortfolioDiff.effectiveBalance shouldBe 0
            totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

            blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
        }
    }
  }

  property("can't trade from scripted account") {

    val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))

    val preconditionsAndExchange
      : Gen[(GenesisTransaction, GenesisTransaction, SetScriptTransaction, IssueTransaction, IssueTransaction, ExchangeTransaction)] = for {
      version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      buyer   <- accountGen
      seller  <- accountGen
      fee     <- smallFeeGen
      ts      <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, seller).map(_._1).retryUntil(_.script.isEmpty)
      issue2: IssueTransaction <- issueReissueBurnGeneratorP(ENOUGH_AMT, buyer).map(_._1).retryUntil(_.script.isEmpty)
      setScript = SetScriptTransaction.selfSigned(version, seller, Some(ScriptV1(TRUE).explicitGet()), fee, ts).explicitGet()
      maybeAsset1 <- Gen.option(issue1.id())
      maybeAsset2 <- Gen.option(issue2.id()) suchThat (x => x != maybeAsset1)
      exchange    <- exchangeGeneratorP(buyer, seller, maybeAsset1, maybeAsset2)
    } yield (gen1, gen2, setScript, issue1, issue2, exchange)

    forAll(preconditionsAndExchange) {
      case ((gen1, gen2, setScript, issue1, issue2, exchange)) =>
        assertLeft(Seq(TestBlock.create(Seq(gen1, gen2, issue1, issue2, setScript))), TestBlock.create(Seq(exchange)), fs)(
          "can't participate in ExchangeTransaction")
    }
  }

  property("buy zbs without enough money for fee") {
    val preconditions: Gen[(GenesisTransaction, GenesisTransaction, IssueTransactionV1, ExchangeTransaction)] = for {
      buyer  <- accountGen
      seller <- accountGen
      ts     <- timestampGen
      gen1: GenesisTransaction = GenesisTransaction.create(buyer, 1 * Constants.UnitsInZbs, ts).explicitGet()
      gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
      issue1: IssueTransactionV1 <- issueGen(buyer)
      exchange                   <- exchangeGeneratorP(buyer, seller, None, Some(issue1.id()), fixedMatcherFee = Some(300000))
    } yield (gen1, gen2, issue1, exchange)

    forAll(preconditions) {
      case ((gen1, gen2, issue1, exchange)) =>
        whenever(exchange.amount > 300000) {
          assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(exchange))) {
            case (blockDiff, state) =>
              val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
              totalPortfolioDiff.balance shouldBe 0
              totalPortfolioDiff.effectiveBalance shouldBe 0
              totalPortfolioDiff.assets.values.toSet shouldBe Set(0L)

              blockDiff.portfolios(exchange.sender).balance shouldBe exchange.buyMatcherFee + exchange.sellMatcherFee - exchange.fee
          }
        }
    }
  }

  def createExTx(buy: Order, sell: Order, price: Long, matcher: PrivateKeyAccount, ts: Long): Either[ValidationError, ExchangeTransaction] = {
    val mf     = buy.matcherFee
    val amount = math.min(buy.amount, sell.amount)
    ExchangeTransaction.create(
      matcher = matcher,
      buyOrder = buy,
      sellOrder = sell,
      price = price,
      amount = amount,
      buyMatcherFee = (BigInt(mf) * amount / buy.amount).toLong,
      sellMatcherFee = (BigInt(mf) * amount / sell.amount).toLong,
      fee = buy.matcherFee,
      timestamp = ts
    )
  }

  property("small fee cases") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller)
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, price, 1000000L, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, price, 1L, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffAndState(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx))) {
          case (blockDiff, state) =>
            blockDiff.portfolios(tx.sender).balance shouldBe tx.buyMatcherFee + tx.sellMatcherFee - tx.fee
            state.portfolio(tx.sender).balance shouldBe 0L
        }
    }
  }

  property("Not enough balance") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[(PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(seller, fixedQuantity = Some(1000L))
      } yield (buyer, seller, matcher, gen1, gen2, issue1)

    forAll(preconditions, priceGen) {
      case ((buyer, seller, matcher, gen1, gen2, issue1), price) =>
        val assetPair = AssetPair(Some(issue1.id()), None)
        val buy       = Order.buy(buyer, matcher, assetPair, price, issue1.quantity + 1, Ts, Ts + 1, MatcherFee)
        val sell      = Order.sell(seller, matcher, assetPair, price, issue1.quantity + 1, Ts, Ts + 1, MatcherFee)
        val tx        = createExTx(buy, sell, price, matcher, Ts).explicitGet()
        assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, issue1))), TestBlock.create(Seq(tx))) { totalDiffEi =>
          inside(totalDiffEi) {
            case Left(TransactionValidationError(AccountBalanceError(errs), _)) =>
              errs should contain key seller.toAddress
          }
        }
    }
  }

  property("Diff for ExchangeTransaction works as expected and doesn't use rounding inside") {
    val MatcherFee = 300000L
    val Ts         = 1000L

    val preconditions: Gen[
      (PrivateKeyAccount, PrivateKeyAccount, PrivateKeyAccount, GenesisTransaction, GenesisTransaction, GenesisTransaction, IssueTransactionV1)] =
      for {
        buyer   <- accountGen
        seller  <- accountGen
        matcher <- accountGen
        ts      <- timestampGen
        gen1: GenesisTransaction = GenesisTransaction.create(buyer, ENOUGH_AMT, ts).explicitGet()
        gen2: GenesisTransaction = GenesisTransaction.create(seller, ENOUGH_AMT, ts).explicitGet()
        gen3: GenesisTransaction = GenesisTransaction.create(matcher, ENOUGH_AMT, ts).explicitGet()
        issue1: IssueTransactionV1 <- issueGen(buyer, fixedQuantity = Some(Long.MaxValue))
      } yield (buyer, seller, matcher, gen1, gen2, gen3, issue1)

    val (buyer, seller, matcher, gen1, gen2, gen3, issue1) = preconditions.sample.get
    val assetPair                                          = AssetPair(None, Some(issue1.id()))

    val buy  = Order.buy(buyer, matcher, assetPair, 238, 3100000000L, Ts, Ts + 1, MatcherFee)
    val sell = Order.sell(seller, matcher, assetPair, 235, 425532L, Ts, Ts + 1, MatcherFee)
    val tx = ExchangeTransaction
      .create(
        matcher = matcher,
        buyOrder = buy,
        sellOrder = sell,
        price = 238,
        amount = 425532,
        buyMatcherFee = 41,
        sellMatcherFee = 300000,
        fee = buy.matcherFee,
        timestamp = Ts
      )
      .explicitGet()

    assertDiffEi(Seq(TestBlock.create(Seq(gen1, gen2, gen3, issue1))), TestBlock.create(Seq(tx))) { totalDiffEi =>
      inside(totalDiffEi) {
        case Right(diff) =>
          import diff.portfolios
          portfolios(buyer).balance shouldBe (-41L + 425532L)
          portfolios(seller).balance shouldBe (-300000L - 425532L)
          portfolios(matcher).balance shouldBe (+41L + 300000L - tx.fee)
      }
    }
  }
}
