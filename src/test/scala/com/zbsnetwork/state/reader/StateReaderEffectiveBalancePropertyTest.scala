package com.zbsplatform.state.reader

import com.zbsplatform.state.EitherExt2
import com.zbsplatform.state.diffs._
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.GenesisTransaction

class StateReaderEffectiveBalancePropertyTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {
  val setup: Gen[(GenesisTransaction, Int, Int, Int)] = for {
    master <- accountGen
    ts     <- positiveIntGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    emptyBlocksAmt <- Gen.choose(1, 10)
    atHeight       <- Gen.choose(1, 20)
    confirmations  <- Gen.choose(1, 20)
  } yield (genesis, emptyBlocksAmt, atHeight, confirmations)

  property("No-interactions genesis account's effectiveBalance doesn't depend on depths") {
    forAll(setup) {
      case (genesis: GenesisTransaction, emptyBlocksAmt, atHeight, confirmations) =>
        val genesisBlock = TestBlock.create(Seq(genesis))
        val nextBlocks   = List.fill(emptyBlocksAmt - 1)(TestBlock.create(Seq.empty))
        assertDiffAndState(genesisBlock +: nextBlocks, TestBlock.create(Seq.empty)) { (_, newState) =>
          newState.effectiveBalance(genesis.recipient, atHeight, confirmations) shouldBe genesis.amount
        }
    }
  }
}
