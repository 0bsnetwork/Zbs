package com.zbsnetwork.lang.contract

import com.zbsnetwork.lang.contract.Contract.{ContractFunction, VerifierFunction}
import com.zbsnetwork.lang.v1.compiler.CompilationError.Generic
import com.zbsnetwork.lang.v1.compiler.Terms.DECLARATION
import com.zbsnetwork.lang.v1.compiler.Types._
import com.zbsnetwork.lang.v1.compiler.{CompilationError, Terms}
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext

/*
 Contact is a list of annotated definitions
 ContractInvokation is (ContractFunction name, ContractFunction args)

 In PoC:
  - No declarations
  - ContractFunctions can't invoke each other
 */

case class Contract(
    dec: List[DECLARATION],
    cfs: List[ContractFunction],
    vf: Option[VerifierFunction]
)

object Contract {

  sealed trait Annotation {
    def dic: Map[String, FINAL]
  }
  object Annotation {
    def parse(name: String, args: List[String]): Either[CompilationError, Annotation] = {
      (name, args) match {
        case ("Callable", s :: Nil)           => Right(CallableAnnotation(s))
        case ("Verifier", s :: Nil)           => Right(VerifierAnnotation(s))
        case _                                => Left(Generic(0, 0, "Annotation not recognized"))
      }
    }

    def validateAnnotationSet(l: List[Annotation]): Either[CompilationError, Unit] = {
      l match {
        case (v: VerifierAnnotation) :: Nil                           => Right(())
        case (c: CallableAnnotation) :: Nil                           => Right(())
        case _                                                        => Left(Generic(0, 0, "Unsupported annotation set"))
      }
    }
  }
  case class CallableAnnotation(invocationArgName: String) extends Annotation {
    lazy val dic = Map(invocationArgName -> com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.Types.invocationType.typeRef)
  }
  case class VerifierAnnotation(txArgName: String) extends Annotation { lazy val dic = Map(txArgName -> ZbsContext.verifierInput.typeRef) }

  sealed trait AnnotatedFunction {
    def annotation: Annotation
    def u: Terms.FUNC
  }
  case class ContractFunction(override val annotation: CallableAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction
  case class VerifierFunction(override val annotation: VerifierAnnotation, override val u: Terms.FUNC) extends AnnotatedFunction
}
