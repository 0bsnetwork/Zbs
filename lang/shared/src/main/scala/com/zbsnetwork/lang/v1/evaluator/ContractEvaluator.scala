package com.zbsnetwork.lang.v1.evaluator
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.lang.ExecutionError
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.contract.Contract.VerifierFunction
import com.zbsnetwork.lang.v1.FunctionHeader
import com.zbsnetwork.lang.v1.compiler.Terms._
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.Bindings
import com.zbsnetwork.lang.v1.evaluator.ctx.{EvaluationContext, LoggedEvaluationContext}
import com.zbsnetwork.lang.v1.task.imports.{raiseError, _}
import com.zbsnetwork.lang.v1.traits.domain.Tx.{ContractTransfer, Pmt}
import com.zbsnetwork.lang.v1.traits.domain.{Ord, Recipient, Tx}

import scala.collection.mutable.ListBuffer

object ContractEvaluator {
  case class Invocation(fc: FUNCTION_CALL, invoker: ByteStr, payment: Option[(Long, Option[ByteStr])], contractAddress: ByteStr)

  def eval(c: Contract, i: Invocation): EvalM[EVALUATED] = {
    val functionName = i.fc.function.asInstanceOf[FunctionHeader.User].name
    c.cfs.find(_.u.name == functionName) match {
      case None => raiseError[LoggedEvaluationContext, ExecutionError, EVALUATED](s"Callable function '$functionName doesn't exist in the contract")
      case Some(f) =>
        val zeroExpr = Right(
          BLOCK(
            LET(
              f.annotation.invocationArgName,
              Bindings
                .buildInvocation(Recipient.Address(i.invoker), i.payment.map { case (a, t) => Pmt(t, a) }, Recipient.Address(i.contractAddress))
            ),
            BLOCK(f.u, i.fc)
          ))

        for {
          ze <- liftEither(zeroExpr)
          expr = c.dec.foldRight(ze)((d, e) => BLOCK(d, e))
          r <- EvaluatorV1.evalExpr(expr)
        } yield r
    }
  }

  def verify(v: VerifierFunction, tx: Tx): EvalM[EVALUATED] = {
    val t = Bindings.transactionObject(tx, proofsEnabled = true)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def verify(v: VerifierFunction, ord: Ord): EvalM[EVALUATED] = {
    val t = Bindings.orderObject(ord, proofsEnabled = true)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }

  def verify(v: VerifierFunction, ct: ContractTransfer): EvalM[EVALUATED] = {
    val t = Bindings.contractTransfer(ct)
    val expr =
      BLOCK(LET(v.annotation.invocationArgName, t), BLOCK(v.u, FUNCTION_CALL(FunctionHeader.User(v.u.name), List(t))))
    EvaluatorV1.evalExpr(expr)
  }


  def apply(ctx: EvaluationContext, c: Contract, i: Invocation): Either[ExecutionError, ContractResult] = {
    val log = ListBuffer[LogItem]()
    val llc = (str: String) => (v: LetExecResult) => log.append((str, v))
    val lec = LoggedEvaluationContext(llc, ctx)
    eval(c, i).run(lec).value._2.flatMap(ContractResult.fromObj)
  }
}