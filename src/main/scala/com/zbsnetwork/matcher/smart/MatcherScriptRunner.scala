package com.zbsnetwork.matcher.smart

import cats.implicits._
import com.zbsnetwork.account.AddressScheme
import com.zbsnetwork.lang.v1.compiler.Terms.EVALUATED
import com.zbsnetwork.lang.v1.evaluator.{EvaluatorV1, Log}
import com.zbsnetwork.transaction.assets.exchange.Order
import com.zbsnetwork.transaction.smart.script.Script
import com.zbsnetwork.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import monix.eval.Coeval

object MatcherScriptRunner {

  def apply(script: Script, order: Order, isTokenScript: Boolean): (Log, Either[String, EVALUATED]) = script match {
    case s: ExprScriprImpl =>
      val ctx = MatcherContext.build(script.version, AddressScheme.current.chainId, Coeval.evalOnce(order), !isTokenScript)
      EvaluatorV1.applywithLogging(ctx, s.expr)
    case _ => (List.empty, "Unsupported script version".asLeft[EVALUATED])
  }
}
