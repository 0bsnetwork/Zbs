package com.zbsnetwork.lang.v1.evaluator.ctx

import cats.data.EitherT
import cats.implicits._
import com.zbsnetwork.lang.TrampolinedExecResult
import com.zbsnetwork.lang.v1.compiler.Terms.EVALUATED
import com.zbsnetwork.lang.v1.evaluator.LogCallback
import monix.eval.Coeval

sealed trait LazyVal {
  val value: TrampolinedExecResult[EVALUATED]
}

object LazyVal {
  private case class LazyValImpl(v: TrampolinedExecResult[EVALUATED], lc: LogCallback) extends LazyVal {
    override val value: TrampolinedExecResult[EVALUATED] = EitherT(
      Coeval.evalOnce(
        v.value
          .flatTap(a => Coeval.evalOnce(lc(a)))
          .apply()
      )
    )
  }

  def apply(v: TrampolinedExecResult[EVALUATED], lc: LogCallback = _ => ()): LazyVal = LazyValImpl(v, lc)
}
