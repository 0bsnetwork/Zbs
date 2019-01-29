package com.zbsnetwork.transaction.smart

import cats.kernel.Monoid
import com.zbsnetwork.lang.Global
import com.zbsnetwork.lang.Version._
import com.zbsnetwork.lang.v1.evaluator.ctx.EvaluationContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.zbsnetwork.state._
import com.zbsnetwork.transaction._
import com.zbsnetwork.transaction.assets.exchange.Order
import monix.eval.Coeval
import shapeless._

object BlockchainContext {

  type In = Transaction :+: Order :+: CNil
  def build(version: Version, nByte: Byte, in: Coeval[In], h: Coeval[Int], blockchain: Blockchain, isTokenContext: Boolean): EvaluationContext = {
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
