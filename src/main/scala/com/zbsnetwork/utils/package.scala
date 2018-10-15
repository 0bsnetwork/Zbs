package com.zbsplatform

import java.security.SecureRandom

import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.zbsplatform.account.AddressScheme
import com.zbsplatform.db.{Storage, VersionedStorage}
import com.zbsplatform.lang.Global
import com.zbsplatform.state._
import com.zbsplatform.lang.v1.compiler.CompilerContext
import com.zbsplatform.lang.v1.compiler.CompilerContext._
import com.zbsplatform.lang.v1.evaluator.ctx._
import com.zbsplatform.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.zbsplatform.lang.v1.{FunctionHeader, ScriptEstimator}
import com.zbsplatform.transaction.smart.{BlockchainContext, ZbsEnvironment}
import monix.eval.Coeval
import monix.execution.UncaughtExceptionReporter
import org.joda.time.Duration
import org.joda.time.format.PeriodFormat

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.duration._
import scala.reflect.runtime.universe
import scala.util.{Failure, Success, Try}

package object utils extends ScorexLogging {

  private val BytesMaxValue  = 256
  private val Base58MaxValue = 58

  private val BytesLog = math.log(BytesMaxValue)
  private val BaseLog  = math.log(Base58MaxValue)

  val UncaughtExceptionsToLogReporter = UncaughtExceptionReporter(exc => log.error(Throwables.getStackTraceAsString(exc)))

  def base58Length(byteArrayLength: Int): Int = math.ceil(BytesLog / BaseLog * byteArrayLength).toInt

  def createWithVerification[A <: Storage with VersionedStorage](storage: => A): Try[A] = Try {
    if (storage.isVersionValid) storage
    else {
      log.info(s"Re-creating storage")
      val b = storage.createBatch()
      storage.removeEverything(b)
      storage.commit(b)
      storage
    }
  }

  def forceStopApplication(reason: ApplicationStopReason = Default): Unit =
    new Thread(() => {
      System.exit(reason.code)
    }, "zbs-platform-shutdown-thread").start()

  def humanReadableSize(bytes: Long, si: Boolean = true): String = {
    val (baseValue, unitStrings) =
      if (si)
        (1000, Vector("B", "kB", "MB", "GB", "TB", "PB", "EB", "ZB", "YB"))
      else
        (1024, Vector("B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"))

    def getExponent(curBytes: Long, baseValue: Int, curExponent: Int = 0): Int =
      if (curBytes < baseValue) curExponent
      else {
        val newExponent = 1 + curExponent
        getExponent(curBytes / (baseValue * newExponent), baseValue, newExponent)
      }

    val exponent   = getExponent(bytes, baseValue)
    val divisor    = Math.pow(baseValue, exponent)
    val unitString = unitStrings(exponent)

    f"${bytes / divisor}%.1f $unitString"
  }

  def humanReadableDuration(duration: Long): String = {
    val d = new Duration(duration)
    PeriodFormat.getDefault.print(d.toPeriod)
  }

  implicit class Tap[A](a: A) {
    def tap(g: A => Unit): A = {
      g(a)
      a
    }
  }

  lazy val dummyNetworkByte: Byte                           = AddressScheme.current.chainId
  lazy val dummyEvaluationContext: EvaluationContext        = BlockchainContext.build(dummyNetworkByte, Coeval(???), Coeval(???), null)
  lazy val functionCosts: Map[FunctionHeader, Coeval[Long]] = estimate(dummyEvaluationContext)

  def estimate(ctx: EvaluationContext): Map[FunctionHeader, Coeval[Long]] = {
    val costs: mutable.Map[FunctionHeader, Coeval[Long]] = ctx.typeDefs.collect {
      case (typeName, CaseType(_, fields)) => FunctionHeader.User(typeName) -> Coeval.now(fields.size.toLong)
    }(collection.breakOut)

    ctx.functions.values.foreach { func =>
      val cost = func match {
        case f: UserFunction =>
          import f.signature.args
          Coeval.evalOnce(ScriptEstimator(ctx.letDefs.keySet ++ args.map(_._1), costs, f.ev).explicitGet() + args.size * 5)
        case f: NativeFunction => Coeval.now(f.cost)
      }
      costs += func.header -> cost
    }

    costs.toMap
  }

  lazy val dummyCompilerContext: CompilerContext =
    Monoid.combineAll(
      Seq(
        CryptoContext.compilerContext(Global),
        ZbsContext.build(new ZbsEnvironment(dummyNetworkByte, Coeval(???), Coeval(???), null)).compilerContext,
        PureContext.compilerContext
      ))

  lazy val dummyVarNames = dummyCompilerContext.varDefs.keySet

  @tailrec
  final def untilTimeout[T](timeout: FiniteDuration, delay: FiniteDuration = 100.milliseconds, onFailure: => Unit = {})(fn: => T): T = {
    Try {
      fn
    } match {
      case Success(x) => x
      case _ if timeout > delay =>
        Thread.sleep(delay.toMillis)
        untilTimeout(timeout - delay, delay, onFailure)(fn)
      case Failure(e) =>
        onFailure
        throw e
    }
  }

  def randomBytes(howMany: Int = 32): Array[Byte] = {
    val r = new Array[Byte](howMany)
    new SecureRandom().nextBytes(r) //overrides r
    r
  }

  def objectFromString[T](fullClassName: String): Try[T] = Try {
    val runtimeMirror = universe.runtimeMirror(getClass.getClassLoader)
    val module        = runtimeMirror.staticModule(fullClassName)
    val obj           = runtimeMirror.reflectModule(module)
    obj.instance.asInstanceOf[T]
  }
}
