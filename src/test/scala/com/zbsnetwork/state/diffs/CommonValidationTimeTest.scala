package com.zbsnetwork.state.diffs

import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.db.WithState
import com.zbsnetwork.settings.TestFunctionalitySettings.Enabled
import com.zbsnetwork.state._
import com.zbsnetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class CommonValidationTimeTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithState {

  property("disallows too old transacions") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs     <- timestampGen
      master      <- accountGen
      height      <- positiveIntGen
      recipient   <- accountGen
      amount      <- positiveLongGen
      fee         <- smallFeeGen
      transfer1 = createZbsTransfer(master, recipient, amount, fee, prevBlockTs - Enabled.maxTransactionTimeBackOffset.toMillis - 1)
        .explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) {
      case (prevBlockTs, blockTs, height, transfer1) =>
        withStateAndHistory(Enabled) { blockchain: Blockchain =>
          TransactionDiffer(Enabled, Some(prevBlockTs), blockTs, height)(blockchain, transfer1) should
            produce("in the past relative to previous block timestamp")
        }
    }
  }

  property("disallows transactions from far future") {
    forAll(for {
      prevBlockTs <- timestampGen
      blockTs     <- Gen.choose(prevBlockTs, prevBlockTs + 7 * 24 * 3600 * 1000)
      master      <- accountGen
      height      <- positiveIntGen
      recipient   <- accountGen
      amount      <- positiveLongGen
      fee         <- smallFeeGen
      transfer1 = createZbsTransfer(master, recipient, amount, fee, blockTs + Enabled.maxTransactionTimeForwardOffset.toMillis + 1)
        .explicitGet()
    } yield (prevBlockTs, blockTs, height, transfer1)) {
      case (prevBlockTs, blockTs, height, transfer1) =>
        val functionalitySettings = Enabled.copy(allowTransactionsFromFutureUntil = blockTs - 1)
        withStateAndHistory(functionalitySettings) { blockchain: Blockchain =>
          TransactionDiffer(functionalitySettings, Some(prevBlockTs), blockTs, height)(blockchain, transfer1) should
            produce("in the future relative to block timestamp")
        }
    }
  }
}
