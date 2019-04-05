package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.lang.ScriptType.ScriptType
import com.zbsnetwork.lang.StdLibVersion.StdLibVersion
import com.zbsnetwork.lang.directives.DirectiveParser
import com.zbsnetwork.lang.utils._
import com.zbsnetwork.lang.v1.ScriptEstimator
import com.zbsnetwork.lang.v1.compiler.{ContractCompiler, ExpressionCompiler}
import com.zbsnetwork.lang.v1.parser.Parser
import com.zbsnetwork.lang.{ScriptType, StdLibVersion}
import com.zbsnetwork.transaction.smart.script.ContractScript._
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.smart.script.v1.ExprScript.ExprScriprImpl
import com.zbsnetwork.utils._

object ScriptCompiler extends ScorexLogging {

  def contract(scriptText: String): Either[String, Script] = {
    val ctx = compilerContext(StdLibVersion.V3, isAssetScript = false)
    ContractCompiler(ctx, Parser.parseContract(scriptText).get.value)
      .flatMap(s => ContractScript(StdLibVersion.V3, s))
  }

  def apply(scriptText: String, isAssetScript: Boolean): Either[String, (Script, Long)] = {
    val directives = DirectiveParser(scriptText)

    val scriptWithoutDirectives =
      scriptText.linesIterator
        .filter(str => !str.contains("{-#"))
        .mkString("\n")

    for {
      ver    <- extractStdLibVersion(directives)
      tpe    <- extractScriptType(directives)
      script <- tryCompile(scriptWithoutDirectives, tpe, ver, isAssetScript)
    } yield (script, script.complexity)
  }

  def tryCompile(src: String, tpe: ScriptType, version: StdLibVersion, isAssetScript: Boolean): Either[String, Script] = {
    val ctx = compilerContext(version, isAssetScript)
    try {
      tpe match {
        case ScriptType.Expression => ExpressionCompiler.compile(src, ctx).flatMap(expr => ExprScript.apply(version, expr))
        case ScriptType.Contract   => ContractCompiler.compile(src, ctx).flatMap(expr => ContractScript.apply(version, expr))
      }
    } catch {
      case ex: Throwable =>
        log.error("Error compiling script", ex)
        log.error(src)
        val msg = Option(ex.getMessage).getOrElse("Parsing failed: Unknown error")
        Left(msg)
    }
  }

  def estimate(script: Script, version: StdLibVersion): Either[String, Long] = script match {
    case s: ExprScriprImpl     => ScriptEstimator(varNames(version), functionCosts(version), s.expr)
    case s: ContractScriptImpl => ContractScript.estimateComplexity(version, s.expr).map(_._2)
    case _                     => ???
  }

}
