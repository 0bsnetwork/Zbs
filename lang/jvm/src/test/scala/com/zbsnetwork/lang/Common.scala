package com.zbsnetwork.lang

import cats.data.EitherT
import cats.kernel.Monoid
import com.zbsnetwork.common.state.diffs.ProduceError
import com.zbsnetwork.lang.Version.ExprV1
import com.zbsnetwork.lang.v1.CTX
import com.zbsnetwork.lang.v1.compiler.Terms._
import com.zbsnetwork.lang.v1.compiler.Types._
import com.zbsnetwork.lang.v1.evaluator.EvaluatorV1
import com.zbsnetwork.lang.v1.evaluator.ctx._
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{EnvironmentFunctions, PureContext, _}
import com.zbsnetwork.lang.v1.traits.domain.{Ord, Recipient, Tx}
import com.zbsnetwork.lang.v1.traits.{DataType, Environment}
import monix.eval.Coeval
import org.scalacheck.Shrink
import shapeless.{:+:, CNil}

import scala.util.{Left, Right, Try}

object Common {
  import com.zbsnetwork.lang.v1.evaluator.ctx.impl.converters._

  private val dataEntryValueType = UNION(LONG, BOOLEAN, BYTESTR, STRING)
  val dataEntryType              = CaseType("DataEntry", List("key" -> STRING, "value" -> dataEntryValueType))
  val addCtx: CTX                = CTX.apply(Seq(dataEntryType), Map.empty, Array.empty)

  def ev[T <: EVALUATED](context: EvaluationContext = Monoid.combine(PureContext.build(ExprV1).evaluationContext, addCtx.evaluationContext),
                         expr: EXPR): Either[ExecutionError, T] =
    EvaluatorV1[T](context, expr)

  trait NoShrink {
    implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
  }

  def produce(errorMessage: String): ProduceError = new ProduceError(errorMessage)

  val multiplierFunction: NativeFunction =
    NativeFunction("MULTIPLY", 1, 10005, LONG, "test ultiplication", ("x1", LONG, "x1"), ("x2", LONG, "x2")) {
      case CONST_LONG(x1: Long) :: CONST_LONG(x2: Long) :: Nil => Try(x1 * x2).map(CONST_LONG).toEither.left.map(_.toString)
      case _                                                   => ??? // suppress pattern match warning
    }

  val pointTypeA = CaseType("PointA", List("X"  -> LONG, "YA" -> LONG))
  val pointTypeB = CaseType("PointB", List("X"  -> LONG, "YB" -> LONG))
  val pointTypeC = CaseType("PointC", List("YB" -> LONG))
  val pointTypeD = CaseType("PointD", List("YB" -> UNION(LONG, UNIT)))

  val AorB    = UNION(pointTypeA.typeRef, pointTypeB.typeRef)
  val AorBorC = UNION(pointTypeA.typeRef, pointTypeB.typeRef, pointTypeC.typeRef)
  val BorC    = UNION(pointTypeB.typeRef, pointTypeC.typeRef)
  val CorD    = UNION(pointTypeC.typeRef, pointTypeD.typeRef)

  val pointAInstance  = CaseObj(pointTypeA.typeRef, Map("X"  -> 3L, "YA" -> 40L))
  val pointBInstance  = CaseObj(pointTypeB.typeRef, Map("X"  -> 3L, "YB" -> 41L))
  val pointCInstance  = CaseObj(pointTypeC.typeRef, Map("YB" -> 42L))
  val pointDInstance1 = CaseObj(pointTypeD.typeRef, Map("YB" -> 43L))

  val pointDInstance2 = CaseObj(pointTypeD.typeRef, Map("YB" -> unit))

  val sampleTypes = Seq(pointTypeA, pointTypeB, pointTypeC, pointTypeD) ++ Seq(UnionType("PointAB", AorB.l),
                                                                               UnionType("PointBC", BorC.l),
                                                                               UnionType("PointCD", CorD.l))

  def sampleUnionContext(instance: CaseObj) =
    EvaluationContext.build(Map.empty, Map("p" -> LazyVal(EitherT.pure(instance))), Seq.empty)

  def emptyBlockchainEnvironment(h: Int = 1, in: Coeval[Tx :+: Ord :+: CNil] = Coeval(???), nByte: Byte = 'T'): Environment = new Environment {
    override def height: Long  = h
    override def chainId: Byte = nByte
    override def inputEntity   = in()

    override def transactionById(id: Array[Byte]): Option[Tx]                                                    = ???
    override def transactionHeightById(id: Array[Byte]): Option[Long]                                            = ???
    override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any]                        = ???
    override def resolveAlias(name: String): Either[String, Recipient.Address]                                   = ???
    override def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long] = ???
  }

  def addressFromPublicKey(chainId: Byte, pk: Array[Byte], addressVersion: Byte = EnvironmentFunctions.AddressVersion): Array[Byte] = {
    val publicKeyHash   = Global.secureHash(pk).take(EnvironmentFunctions.HashLength)
    val withoutChecksum = addressVersion +: chainId +: publicKeyHash
    withoutChecksum ++ Global.secureHash(withoutChecksum).take(EnvironmentFunctions.ChecksumLength)
  }

  def addressFromString(chainId: Byte, str: String): Either[String, Option[Array[Byte]]] = {
    val base58String = if (str.startsWith(EnvironmentFunctions.AddressPrefix)) str.drop(EnvironmentFunctions.AddressPrefix.length) else str
    Global.base58Decode(base58String, Global.MaxAddressLength) match {
      case Left(e) => Left(e)
      case Right(addressBytes) =>
        val version = addressBytes.head
        val network = addressBytes.tail.head
        lazy val checksumCorrect = {
          val checkSum = addressBytes.takeRight(EnvironmentFunctions.ChecksumLength)
          val checkSumGenerated =
            Global.secureHash(addressBytes.dropRight(EnvironmentFunctions.ChecksumLength)).take(EnvironmentFunctions.ChecksumLength)
          checkSum sameElements checkSumGenerated
        }

        if (version == EnvironmentFunctions.AddressVersion && network == chainId && addressBytes.length == EnvironmentFunctions.AddressLength && checksumCorrect)
          Right(Some(addressBytes))
        else Right(None)
    }
  }
}
