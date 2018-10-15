package com.zbsplatform.state.diffs.smart.scenarios

import com.zbsplatform.account.{AddressOrAlias, AddressScheme, PrivateKeyAccount}
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.evaluator.EvaluatorV1
import com.zbsplatform.lang.v1.evaluator.ctx.CaseObj
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.state._
import com.zbsplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState, produce}
import com.zbsplatform.transaction.smart.BlockchainContext
import com.zbsplatform.transaction.transfer._
import com.zbsplatform.transaction.{CreateAliasTransaction, GenesisTransaction, Transaction}
import com.zbsplatform.{NoShrink, TransactionGen}
import fastparse.core.Parsed
import monix.eval.Coeval
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.ByteVector

class AddressFromRecipientScenarioTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  val preconditionsAndAliasCreations: Gen[(Seq[GenesisTransaction], CreateAliasTransaction, TransferTransactionV1, TransferTransactionV1)] = for {
    master                   <- accountGen
    ts                       <- timestampGen
    other: PrivateKeyAccount <- accountGen
    genesis1: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
    genesis2: GenesisTransaction = GenesisTransaction.create(other, ENOUGH_AMT, ts).explicitGet()
    alias              <- aliasGen
    fee                <- smallFeeGen
    aliasTx            <- createAliasGen(other, alias, fee, ts)
    transferViaAddress <- transferGeneratorP(master, other, None, None)
    transferViaAlias   <- transferGeneratorP(master, AddressOrAlias.fromBytes(alias.bytes.arr, 0).explicitGet()._1, None, None)
  } yield (Seq(genesis1, genesis2), aliasTx, transferViaAddress, transferViaAlias)

  def evalScript(tx: Transaction, blockchain: Blockchain): Either[com.zbsplatform.lang.ExecutionError, CaseObj] = {
    val context =
      BlockchainContext.build(AddressScheme.current.chainId, Coeval.evalOnce(tx), Coeval.evalOnce(blockchain.height), blockchain)

    val Parsed.Success(expr, _) = Parser("""
        | match tx {
        |  case t : TransferTransaction =>  addressFromRecipient(t.recipient)
        |  case other => throw()
        |  }
        |  """.stripMargin)
    val Right((typedExpr, _))   = CompilerV1(com.zbsplatform.utils.dummyCompilerContext, expr)
    EvaluatorV1[CaseObj](context, typedExpr)
  }

  property("Script can resolve AddressOrAlias") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, aliasTx, transferViaAddress, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq(aliasTx))) {
          case (_, state) =>
            val addressBytes = evalScript(transferViaAddress, state).explicitGet().fields("bytes").asInstanceOf[ByteVector]
            addressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true
            val resolvedAddressBytes = evalScript(transferViaAlias, state).explicitGet().fields("bytes").asInstanceOf[ByteVector]

            resolvedAddressBytes.toArray.sameElements(transferViaAddress.recipient.bytes.arr) shouldBe true
        }
    }
  }

  property("Script can't resolve alias that doesn't exist") {
    forAll(preconditionsAndAliasCreations) {
      case (gen, _, _, transferViaAlias) =>
        assertDiffAndState(Seq(TestBlock.create(gen)), TestBlock.create(Seq())) {
          case (_, state) =>
            evalScript(transferViaAlias, state) should produce("AliasDoesNotExist")
        }
    }
  }
}
