package com.zbsnetwork.transaction.smart.script

import cats.implicits._
import com.zbsnetwork.account.AddressScheme
import com.zbsnetwork.lang.v1.compiler.Terms.EVALUATED
import com.zbsnetwork.lang.v1.evaluator.EvaluatorV1
import com.zbsnetwork.lang._
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.v1.evaluator._
import com.zbsnetwork.lang.v1.compiler.Terms.{FALSE, TRUE}
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.{Authorized, Proven, Transaction}
import com.zbsnetwork.transaction.assets.exchange.Order
import com.zbsnetwork.transaction.smart.{BlockchainContext, RealTransactionWrapper, Verifier}
import com.zbsnetwork.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import com.zbsnetwork.transaction.smart.script.v1.ContractScript
import monix.eval.Coeval
import shapeless._

object ScriptRunner {

  def apply(height: Int,
            in: Transaction :+: Order :+: CNil,
            blockchain: Blockchain,
            script: Script,
            isTokenScript: Boolean): (Log, Either[ExecutionError, EVALUATED]) = {
    script match {
      case s: ExprScriprImpl =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        EvaluatorV1.applywithLogging[EVALUATED](ctx, s.expr)
      case ContractScript(_, Contract(_, _, Some(vf))) =>
        val ctx = BlockchainContext.build(
          script.version,
          AddressScheme.current.chainId,
          Coeval.evalOnce(in),
          Coeval.evalOnce(height),
          blockchain,
          isTokenScript
        )
        val evalContract = ContractEvaluator.verify(vf, in.eliminate(RealTransactionWrapper.apply, _.eliminate(???, ???)))
        EvaluatorV1.evalWithLogging(ctx, evalContract)

      case ContractScript(_, Contract(_, _, None)) =>
        val t: Proven with Authorized = in.eliminate(_.asInstanceOf[Proven with Authorized], _.eliminate(???, ???))
        (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](t) match {
          case Right(_) => Right(TRUE)
          case Left(_)  => Right(FALSE)
        })
      case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
    }
  }
}
