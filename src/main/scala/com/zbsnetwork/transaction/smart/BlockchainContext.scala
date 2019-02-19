package com.zbsnetwork.transaction.smart

import cats.kernel.Monoid
import com.zbsnetwork.lang.Global
import com.zbsnetwork.lang.StdLibVersion._
import com.zbsnetwork.lang.v1.evaluator.ctx.EvaluationContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.zbsnetwork.state._
import monix.eval.Coeval

object BlockchainContext {

  type In = ZbsEnvironment.In
  def build(version: StdLibVersion,
            nByte: Byte,
            in: Coeval[In],
            h: Coeval[Int],
            blockchain: Blockchain,
            isTokenContext: Boolean): EvaluationContext = {
    Monoid
      .combineAll(
        Seq(
          PureContext.build(version),
          CryptoContext.build(Global),
          ZbsContext.build(version, new ZbsEnvironment(nByte, in, h, blockchain), isTokenContext)
        ))
      .evaluationContext
  }

}
