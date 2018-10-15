package com.zbsplatform.transaction.smart.script

import cats.implicits._
import com.zbsplatform.account.AddressScheme
import com.zbsplatform.lang.v1.evaluator.EvaluatorV1
import com.zbsplatform.lang.{ExecutionError, ExprEvaluator}
import com.zbsplatform.state._
import com.zbsplatform.transaction.Transaction
import com.zbsplatform.transaction.smart.BlockchainContext
import monix.eval.Coeval

object ScriptRunner {

  def apply[A, T <: Transaction](height: Int, tx: T, blockchain: Blockchain, script: Script): (ExprEvaluator.Log, Either[ExecutionError, A]) =
    script match {
      case Script.Expr(expr) =>
        val ctx = BlockchainContext.build(
          AddressScheme.current.chainId,
          Coeval.evalOnce(tx),
          Coeval.evalOnce(height),
          blockchain
        )
        EvaluatorV1.applywithLogging[A](ctx, expr)

      case _ => (List.empty, "Unsupported script version".asLeft[A])
    }

}
