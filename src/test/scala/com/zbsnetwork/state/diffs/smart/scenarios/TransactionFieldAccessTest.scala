package com.zbsnetwork.state.diffs.smart.scenarios

import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.lang.Version.ExprV1
import com.zbsnetwork.lang.v1.compiler.ExpressionCompilerV1
import com.zbsnetwork.lang.v1.parser.Parser
import com.zbsnetwork.state.diffs.smart._
import com.zbsnetwork.state.diffs.{assertDiffAndState, assertDiffEi, produce}
import com.zbsnetwork.transaction.GenesisTransaction
import com.zbsnetwork.transaction.lease.LeaseTransaction
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.transfer._
import com.zbsnetwork.utils.compilerContext
import com.zbsnetwork.{NoShrink, TransactionGen}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class TransactionFieldAccessTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  private def preconditionsTransferAndLease(
      code: String): Gen[(GenesisTransaction, SetScriptTransaction, LeaseTransaction, TransferTransactionV2)] = {
    val untyped = Parser.parseScript(code).get.value
    val typed   = ExpressionCompilerV1(compilerContext(ExprV1, isAssetScript = false), untyped).explicitGet()._1
    preconditionsTransferAndLease(typed)
  }

  private val script =
    """
      |
      | match tx {
      | case ttx: TransferTransaction =>
      |       isDefined(ttx.assetId)==false
      |   case other =>
      |       false
      | }
      """.stripMargin

  property("accessing field of transaction without checking its type first results on exception") {
    forAll(preconditionsTransferAndLease(script)) {
      case ((genesis, script, lease, transfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }
}
