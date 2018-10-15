package com.zbsplatform.state

import java.util.concurrent.TimeUnit

import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.settings.FunctionalitySettings
import com.zbsplatform.state.StateSyntheticBenchmark._
import com.zbsplatform.utils.dummyCompilerContext
import org.openjdk.jmh.annotations._
import org.scalacheck.Gen
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.transaction.Transaction
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.transfer._

@OutputTimeUnit(TimeUnit.MILLISECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StateSyntheticBenchmark {

  @Benchmark
  def appendBlock_test(db: St): Unit = db.genAndApplyNextBlock()

  @Benchmark
  def appendBlock_smart_test(db: SmartSt): Unit = db.genAndApplyNextBlock()

}

object StateSyntheticBenchmark {

  @State(Scope.Benchmark)
  class St extends BaseState {
    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        amount    <- Gen.choose(1, zbs(1))
        recipient <- accountGen
      } yield TransferTransactionV1.selfSigned(None, sender, recipient, amount, ts, None, 100000, Array.emptyByteArray).explicitGet()
  }

  @State(Scope.Benchmark)
  class SmartSt extends BaseState {

    override protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = {
      base.copy(preActivatedFeatures = Map(4.toShort -> 0))
    }

    protected override def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction] =
      for {
        recipient: PrivateKeyAccount <- accountGen
        amount                       <- Gen.choose(1, zbs(1))
      } yield
        TransferTransactionV2
          .selfSigned(
            TransferTransactionV2.supportedVersions.head,
            None,
            sender,
            recipient.toAddress,
            amount,
            ts,
            None,
            1000000,
            Array.emptyByteArray
          )
          .explicitGet()

    @Setup
    override def init(): Unit = {
      super.init()

      val textScript    = "sigVerify(tx.bodyBytes,tx.proofs[0],tx.senderPk)"
      val untypedScript = Parser(textScript).get.value
      val typedScript   = CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1

      val setScriptBlock = nextBlock(
        Seq(
          SetScriptTransaction
            .selfSigned(
              SetScriptTransaction.supportedVersions.head,
              richAccount,
              Some(ScriptV1(typedScript).explicitGet()),
              1000000,
              System.currentTimeMillis()
            )
            .explicitGet()
        )
      )

      applyBlock(setScriptBlock)
    }
  }

}
