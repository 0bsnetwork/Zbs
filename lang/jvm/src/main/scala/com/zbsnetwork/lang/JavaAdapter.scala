package com.zbsnetwork.lang

import cats.kernel.Monoid
import com.zbsnetwork.lang.Version.ExprV1
import com.zbsnetwork.lang.v1.compiler.ExpressionCompilerV1
import com.zbsnetwork.lang.v1.compiler.Terms.EXPR
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = ExprV1

  lazy val compiler =
    new ExpressionCompilerV1(
      Monoid.combineAll(Seq(
        CryptoContext.compilerContext(com.zbsnetwork.lang.Global),
        ZbsContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
      )))

  def compile(input: String): EXPR = {
    compiler
      .compile(input, List())
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
