package com.zbsnetwork.transaction.smart.script

import cats.implicits._
import com.zbsnetwork.account.AddressScheme
import com.zbsnetwork.lang._
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.zbsnetwork.lang.v1.evaluator.{EvaluatorV1, _}
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.smart.{BlockchainContext, RealTransactionWrapper, Verifier}
import com.zbsnetwork.transaction.{Authorized, Proven}
import monix.eval.Coeval

object ScriptRunner {
  type TxOrd = BlockchainContext.In

  def apply(height: Int, in: TxOrd, blockchain: Blockchain, script: Script, isTokenScript: Boolean): (Log, Either[ExecutionError, EVALUATED]) = {
    script match {
      case s: ExprScript =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[EVALUATED](ctx, s.expr)
      case ContractScript.ContractScriptImpl(_, Contract(_, _, Some(vf)), _) =>
        val ctx = BlockchainContext.build(
          script.stdLibVersion,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        val evalContract = in.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.apply(t)),
                                        _.eliminate(t => ContractEvaluator.verify(vf, RealTransactionWrapper.ord(t)), _ => ???))
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript.ContractScriptImpl(_, Contract(_, _, None), _) =>
        val t: Proven with Authorized =
          in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(_.asInstanceOf[Proven with Authorized], _ => ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_) => Right(TRUE)
          case Left(_)  => Right(FALSE)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
  }
}
