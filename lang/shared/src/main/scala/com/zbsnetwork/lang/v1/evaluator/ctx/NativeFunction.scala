package com.zbsplatform.lang.v1.evaluator.ctx

import cats.data.EitherT
import com.zbsplatform.lang.TrampolinedExecResult
import com.zbsplatform.lang.v1.FunctionHeader
import com.zbsplatform.lang.v1.compiler.Terms.EXPR
import com.zbsplatform.lang.v1.compiler.Types._
import monix.eval.Coeval

sealed trait BaseFunction {
  def signature: FunctionTypeSignature
  def header: FunctionHeader = signature.header
  def name: String
}

object BaseFunction {
  implicit def header(bf: BaseFunction): FunctionHeader = bf.header
}

case class FunctionTypeSignature(result: TYPE, args: Seq[(String, TYPE)], header: FunctionHeader)

case class NativeFunction private (name: String, cost: Long, signature: FunctionTypeSignature, ev: List[Any] => Either[String, Any])
    extends BaseFunction {
  def eval(args: List[Any]): TrampolinedExecResult[Any] = EitherT.fromEither[Coeval](ev(args))
}

object NativeFunction {

  def apply(name: String, cost: Long, internalName: Short, resultType: TYPE, args: (String, TYPE)*)(ev: List[Any] => Either[String, Any]) =
    new NativeFunction(name, cost, FunctionTypeSignature(resultType, args, FunctionHeader.Native(internalName)), ev)

}

case class UserFunction private (name: String, internalName: String, signature: FunctionTypeSignature, ev: EXPR) extends BaseFunction

object UserFunction {

  def apply(name: String, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    UserFunction(name, name, resultType, args: _*)(ev)

  def apply(name: String, internalName: String, resultType: TYPE, args: (String, TYPE)*)(ev: EXPR): UserFunction =
    new UserFunction(name, internalName, FunctionTypeSignature(resultType, args, FunctionHeader.User(internalName)), ev)
}
