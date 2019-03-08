package com.zbsnetwork.matcher.smart

import cats.implicits._
import com.zbsnetwork.account.AddressScheme
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.v1.compiler.Terms.{EVALUATED, FALSE, TRUE}
import com.zbsnetwork.lang.v1.evaluator.{ContractEvaluator, EvaluatorV1, Log}
import com.zbsnetwork.transaction.assets.exchange.Order
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.smart.script.{ContractScript, Script}
import com.zbsnetwork.transaction.smart.{RealTransactionWrapper, Verifier}
import com.zbsnetwork.transaction.{Authorized, Proven}
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScript =>
      val ctx = MatcherContext.build(script.stdLibVersion, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)

    case ContractScript.ContractScriptImpl(_, Contract(_, _, Some(vf)), _) =>
      val ctx = MatcherContext.build(
        script.stdLibVersion,
        AddressScheme.current.chainId,
        Coeval.evalOnce(???) /*order not used in global context where @Verifier annotation is used */,
        proofsEnabled = true
      )
      val evalContract = ContractEvaluator.verify(vf, RealTransactionWrapper.ord(order))
      EvaluatorV1.evalWithLogging(ctx, evalContract)

    case ContractScript.ContractScriptImpl(_, Contract(_, _, None), _) =>
      (List.empty, Verifier.verifyAsEllipticCurveSignature[Proven with Authorized](order) match {
        case Right(_) => Right(TRUE)
        case Left(_)  => Right(FALSE)
      })
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
