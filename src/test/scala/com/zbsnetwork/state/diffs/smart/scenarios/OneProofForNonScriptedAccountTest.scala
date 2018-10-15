package com.zbsplatform.state.diffs.smart.scenarios

import com.zbsplatform.lang.v1.compiler.Terms._
import com.zbsplatform.state._
import com.zbsplatform.state.diffs.smart.smartEnabledFS
import com.zbsplatform.state.diffs.{ENOUGH_AMT, assertDiffEi, produce}
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.transfer._
import com.zbsplatform.transaction.{GenesisTransaction, Proofs}

class OneProofForNonScriptedAccountTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("exactly 1 proof required for non-scripted accounts") {
    val s = for {
      version   <- Gen.oneOf(TransferTransactionV2.supportedVersions.toSeq)
      master    <- accountGen
      recepient <- accountGen
      amt       <- positiveLongGen
      fee       <- smallFeeGen
      ts        <- positiveIntGen
      genesis = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      setScript <- selfSignedSetScriptTransactionGenP(master, ScriptV1(TRUE).explicitGet())
      transfer = TransferTransactionV2.selfSigned(version, None, master, recepient, amt, ts, None, fee, Array.emptyByteArray).explicitGet()
    } yield (genesis, setScript, transfer)

    forAll(s) {
      case ((genesis, script, transfer)) =>
        val transferWithExtraProof = transfer.copy(proofs = Proofs(Seq(ByteStr.empty, ByteStr(Array(1: Byte)))))
        assertDiffEi(Seq(TestBlock.create(Seq(genesis))), TestBlock.create(Seq(transferWithExtraProof)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("must have exactly 1 proof"))
    }
  }

}
