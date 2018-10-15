package com.zbsplatform.state.diffs.smart.scenarios

import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.state._
import com.zbsplatform.state.diffs._
import com.zbsplatform.state.diffs.smart._
import com.zbsplatform.utils.dummyCompilerContext
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.zbsplatform.lagonaki.mocks.TestBlock

class OnlyTransferIsAllowedTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  property("transfer is allowed but lease is not due to predicate") {

    val scriptText =
      s"""
         |
         | match tx {
         |  case ttx: TransferTransaction | MassTransferTransaction =>
         |     sigVerify(ttx.bodyBytes,ttx.proofs[0],ttx.senderPublicKey)
         |  case other =>
         |     false
         | }
      """.stripMargin
    val untyped         = Parser(scriptText).get.value
    val transferAllowed = CompilerV1(dummyCompilerContext, untyped).explicitGet()._1

    forAll(preconditionsTransferAndLease(transferAllowed)) {
      case (genesis, script, lease, transfer) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(transfer)), smartEnabledFS) { case _ => () }
        assertDiffEi(Seq(TestBlock.create(Seq(genesis, script))), TestBlock.create(Seq(lease)), smartEnabledFS)(totalDiffEi =>
          totalDiffEi should produce("TransactionNotAllowedByScript"))
    }
  }

}
