package com.zbsnetwork.lang

import cats.implicits._
import com.zbsnetwork.lang.ScriptType.ScriptType
import com.zbsnetwork.lang.StdLibVersion.StdLibVersion
import com.zbsnetwork.lang.directives.{Directive, DirectiveKey}

import scala.util.{Failure, Success, Try}

package object utils {

  def extractStdLibVersion(directives: List[Directive]): Either[String, StdLibVersion] = {
    directives
      .find(_.key == DirectiveKey.STDLIB_VERSION)
      .map(d =>
        Try(d.value.toInt) match {
          case Success(v) =>
            val ver = StdLibVersion(v)
            Either
              .cond(
                StdLibVersion.SupportedVersions(ver),
                ver,
                "Unsupported language version"
              )
          case Failure(ex) =>
            Left("Can't parse language version")
      })
      .getOrElse(StdLibVersion.V2.asRight)
  }

  def extractScriptType(directives: List[Directive]): Either[String, ScriptType] = {
    directives
      .find(_.key == DirectiveKey.SCRIPT_TYPE)
      .map(d =>
        Try(d.value) match {
          case Success(v) =>
            val ver = ScriptType.parseString(v)
            Either
              .cond(
                ScriptType.SupportedVersions(ver),
                ver,
                "Unsupported script type"
              )
          case Failure(ex) =>
            Left("Can't parse script type")
      })
      .getOrElse(ScriptType.Expression.asRight)
  }

}
