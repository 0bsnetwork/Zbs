package com.zbsplatform.transaction.smart

import cats.kernel.Monoid
import com.zbsplatform.lang.Global
import com.zbsplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.zbsplatform.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.zbsplatform.state._
import monix.eval.Coeval
import com.zbsplatform.transaction._

object BlockchainContext {

  private val baseContext = Monoid.combine(PureContext.ctx, CryptoContext.build(Global)).evaluationContext

  def build(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain): EvaluationContext =
    Monoid.combine(baseContext, ZbsContext.build(new ZbsEnvironment(nByte, tx, h, blockchain)).evaluationContext)
}
