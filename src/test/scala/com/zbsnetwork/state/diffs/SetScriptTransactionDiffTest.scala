package com.zbsplatform.state.diffs

import com.zbsplatform.features.BlockchainFeatures
import com.zbsplatform.settings.TestFunctionalitySettings
import com.zbsplatform.state._
import com.zbsplatform.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.GenesisTransaction
import com.zbsplatform.transaction.smart.SetScriptTransaction

class SetScriptTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))

  val preconditionsAndSetScript: Gen[(GenesisTransaction, SetScriptTransaction)] = for {
    version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
    master  <- accountGen
    ts      <- timestampGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    fee    <- smallFeeGen
    script <- Gen.option(scriptGen)
  } yield (genesis, SetScriptTransaction.selfSigned(version, master, script, fee, ts).explicitGet())

  property("setting script results in account state") {
    forAll(preconditionsAndSetScript) {
      case (genesis, setScript) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(setScript)), fs) {
          case (blockDiff, newState) =>
            newState.accountScript(setScript.sender) shouldBe setScript.script
        }
    }
  }
}
