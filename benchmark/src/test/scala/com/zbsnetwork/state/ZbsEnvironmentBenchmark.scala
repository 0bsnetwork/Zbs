package com.zbsnetwork.state

import java.io.File
import java.util.concurrent.{ThreadLocalRandom, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.zbsnetwork.account.{AddressOrAlias, AddressScheme, Alias}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.{Base58, EitherExt2}
import com.zbsnetwork.database.LevelDBWriter
import com.zbsnetwork.db.LevelDBFactory
import com.zbsnetwork.lang.v1.traits.Environment
import com.zbsnetwork.lang.v1.traits.domain.Recipient
import com.zbsnetwork.settings.{ZbsSettings, loadConfig}
import com.zbsnetwork.state.ZbsEnvironmentBenchmark._
import com.zbsnetwork.state.bench.DataTestData
import com.zbsnetwork.transaction.smart.ZbsEnvironment
import monix.eval.Coeval
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import scodec.bits.BitVector

import scala.io.Codec

/**
  * Tests over real database. How to test:
  * 1. Download a database
  * 2. Import it: https://github.com/0bsnetwork/Zbs/wiki/Export-and-import-of-the-blockchain#import-blocks-from-the-binary-file
  * 3. Run ExtractInfo to collect queries for tests
  * 4. Make Caches.MaxSize = 1
  * 5. Run this test
  */
@OutputTimeUnit(TimeUnit.NANOSECONDS)
@BenchmarkMode(Array(Mode.AverageTime))
@Threads(1)
@Fork(1)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ZbsEnvironmentBenchmark {

  @Benchmark
  def resolveAddress_test(st: ResolveAddressSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.resolveAlias(st.aliases.random))
  }

  @Benchmark
  def transactionById_test(st: TransactionByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.transactionById(st.allTxs.random))
  }

  @Benchmark
  def transactionHeightById_test(st: TransactionByIdSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.transactionById(st.allTxs.random))
  }

  @Benchmark
  def accountBalanceOf_zbs_test(st: AccountBalanceOfZbsSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.accountBalanceOf(Recipient.Address(ByteStr(st.accounts.random)), None))
  }

  @Benchmark
  def accountBalanceOf_asset_test(st: AccountBalanceOfAssetSt, bh: Blackhole): Unit = {
    bh.consume(st.environment.accountBalanceOf(Recipient.Address(ByteStr(st.accounts.random)), Some(st.assets.random)))
  }

  @Benchmark
  def data_test(st: DataSt, bh: Blackhole): Unit = {
    val x = st.data.random
    bh.consume(st.environment.data(Recipient.Address(x.addr), x.key, x.dataType))
  }

}

object ZbsEnvironmentBenchmark {

  @State(Scope.Benchmark)
  class ResolveAddressSt extends BaseSt {
    val aliases: Vector[String] = load("resolveAddress", benchSettings.aliasesFile)(x => Alias.fromString(x).explicitGet().name)
  }

  @State(Scope.Benchmark)
  class TransactionByIdSt extends BaseSt {
    val allTxs: Vector[Array[Byte]] = load("transactionById", benchSettings.restTxsFile)(x => Base58.decode(x).get)
  }

  @State(Scope.Benchmark)
  class TransactionHeightByIdSt extends TransactionByIdSt

  @State(Scope.Benchmark)
  class AccountBalanceOfZbsSt extends BaseSt {
    val accounts: Vector[Array[Byte]] = load("accounts", benchSettings.accountsFile)(x => AddressOrAlias.fromString(x).explicitGet().bytes.arr)
  }

  @State(Scope.Benchmark)
  class AccountBalanceOfAssetSt extends AccountBalanceOfZbsSt {
    val assets: Vector[Array[Byte]] = load("assets", benchSettings.assetsFile)(x => Base58.decode(x).get)
  }

  @State(Scope.Benchmark)
  class DataSt extends BaseSt {
    val data: Vector[DataTestData] = load("data", benchSettings.dataFile) { line =>
      DataTestData.codec.decode(BitVector.fromBase64(line).get).require.value
    }
  }

  @State(Scope.Benchmark)
  class BaseSt {
    protected val benchSettings: Settings = Settings.fromConfig(ConfigFactory.load())
    private val zbsSettings: ZbsSettings = {
      val config = loadConfig(ConfigFactory.parseFile(new File(benchSettings.networkConfigFile)))
      ZbsSettings.fromConfig(config)
    }

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = zbsSettings.blockchainSettings.addressSchemeCharacter.toByte
    }

    private val db: DB = {
      val dir = new File(zbsSettings.dataDirectory)
      if (!dir.isDirectory) throw new IllegalArgumentException(s"Can't find directory at '${zbsSettings.dataDirectory}'")
      LevelDBFactory.factory.open(dir, new Options)
    }

    val environment: Environment = {
      val state = new LevelDBWriter(db, zbsSettings.blockchainSettings.functionalitySettings, 100000, 2000, 120 * 60 * 1000)
      new ZbsEnvironment(
        AddressScheme.current.chainId,
        Coeval.raiseError(new NotImplementedError("tx is not implemented")),
        Coeval(state.height),
        state
      )
    }

    @TearDown
    def close(): Unit = {
      db.close()
    }

    protected def load[T](label: String, absolutePath: String)(f: String => T): Vector[T] = {
      scala.io.Source
        .fromFile(absolutePath)(Codec.UTF8)
        .getLines()
        .map(f)
        .toVector
    }
  }

  implicit class VectorOps[T](self: Vector[T]) {
    def random: T = self(ThreadLocalRandom.current().nextInt(self.size))
  }

}
