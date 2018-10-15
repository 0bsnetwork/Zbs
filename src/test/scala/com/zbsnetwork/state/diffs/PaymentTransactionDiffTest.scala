package com.zbsplatform.state.diffs

import cats.Monoid
import com.zbsplatform.settings.TestFunctionalitySettings
import com.zbsplatform.state._
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.{GenesisTransaction, PaymentTransaction}

class PaymentTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndPayments: Gen[(GenesisTransaction, PaymentTransaction, PaymentTransaction)] = for {
    master    <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    paymentV2: PaymentTransaction <- paymentGeneratorP(master, recipient)
    paymentV3: PaymentTransaction <- paymentGeneratorP(master, recipient)
  } yield (genesis, paymentV2, paymentV3)

  val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 2)

  property("Diff doesn't break invariant before block version 3") {
    forAll(preconditionsAndPayments) {
      case ((genesis, paymentV2, _)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(paymentV2)), settings) { (blockDiff, newState) =>
          val totalPortfolioDiff: Portfolio = Monoid.combineAll(blockDiff.portfolios.values)
          totalPortfolioDiff.balance shouldBe 0
          totalPortfolioDiff.effectiveBalance shouldBe 0
        }
    }
  }

  property("Validation fails with block version 3") {
    forAll(preconditionsAndPayments) {
      case ((genesis, paymentV2, paymentV3)) =>
        assertDiffEi(Seq(TestBlock.create(Seq(genesis)), TestBlock.create(Seq(paymentV2))), TestBlock.create(Seq(paymentV3)), settings) {
          blockDiffEi =>
            blockDiffEi should produce(s"Payment transaction is deprecated after h=${settings.blockVersion3AfterHeight}")
        }
    }
  }

}
