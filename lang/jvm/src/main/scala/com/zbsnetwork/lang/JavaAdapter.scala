package com.zbsnetwork.lang

import cats.kernel.Monoid
import com.zbsnetwork.lang.StdLibVersion.V2
import com.zbsnetwork.lang.v1.compiler.ExpressionCompiler
import com.zbsnetwork.lang.v1.compiler.Terms.EXPR
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}

object JavaAdapter {
  private val version = V2

  lazy val ctx =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(com.zbsnetwork.lang.Global),
        ZbsContext.build(version, null, false).compilerContext,
        PureContext.build(version).compilerContext
      ))

  def compile(input: String): EXPR = {
    ExpressionCompiler
      .compile(input, ctx)
      .fold(
        error => throw new IllegalArgumentException(error),
        expr => expr
      )
  }
}
