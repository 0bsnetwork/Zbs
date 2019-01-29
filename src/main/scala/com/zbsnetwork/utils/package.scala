package com.zbsnetwork

import java.security.SecureRandom

import cats.kernel.Monoid
import com.google.common.base.Throwables
import com.zbsnetwork.account.AddressScheme
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.state.ByteStr._
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.db.{Storage, VersionedStorage}
import com.zbsnetwork.lang.Global
import com.zbsnetwork.lang.Version._
import com.zbsnetwork.lang.v1.compiler.CompilerContext
import com.zbsnetwork.lang.v1.evaluator.ctx._
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.ZbsContext
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.zbsnetwork.lang.v1.{CTX, FunctionHeader, ScriptEstimator}
import com.zbsnetwork.transaction.smart.ZbsEnvironment
import monix.eval.Coeval
import monix.execution.UncaughtExceptionReporter
import org.joda.time.Duration
import org.joda.time.format.PeriodFormat
import play.api.libs.json._

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

  private val lazyAssetContexts: Map[Version, Coeval[CTX]] =
    Seq
      .tabulate(2) { v =>
        val version = com.zbsnetwork.lang.Version.parseVersion(v + 1)
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(Global),
              ZbsContext
                .build(version, new ZbsEnvironment(AddressScheme.current.chainId, Coeval(???), Coeval(???), EmptyBlockchain), isTokenContext = true)
            )))
      }
      .toMap

  private val lazyContexts: Map[Version, Coeval[CTX]] =
    Seq
      .tabulate(3) { v =>
        val version: Version = com.zbsnetwork.lang.Version(v + 1)
        version -> Coeval.evalOnce(
          Monoid
            .combineAll(Seq(
              PureContext.build(version),
              CryptoContext.build(Global),
              ZbsContext
                .build(version, new ZbsEnvironment(AddressScheme.current.chainId, Coeval(???), Coeval(???), EmptyBlockchain), isTokenContext = false)
            )))
      }
      .toMap

  def dummyEvalContext(version: Version): EvaluationContext = lazyContexts(version)().evaluationContext

  private val lazyFunctionCosts: Map[Version, Coeval[Map[FunctionHeader, Coeval[Long]]]] =
    lazyContexts.mapValues(_.map(ctx => estimate(ctx.evaluationContext)))

  def functionCosts(version: Version): Map[FunctionHeader, Coeval[Long]] = lazyFunctionCosts(version)()

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

  def compilerContext(version: Version, isAssetScript: Boolean): CompilerContext =
    if (isAssetScript) lazyAssetContexts(version)().compilerContext
    else lazyContexts(version)().compilerContext

  def varNames(version: Version): Set[String] = compilerContext(version, isAssetScript = false).varDefs.keySet

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

  implicit val byteStrWrites: Format[ByteStr] = new Format[ByteStr] {

    override def writes(o: ByteStr): JsValue = JsString(o.base58)

    override def reads(json: JsValue): JsResult[ByteStr] = json match {
      case JsString(v) => decodeBase58(v).fold(e => JsError(s"Error parsing base58: ${e.getMessage}"), b => JsSuccess(b))
      case _           => JsError("Expected JsString")
    }
  }
}
