package com.zbsnetwork.state

import java.io.File
import java.nio.file.Files

import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.block.Block
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.database.LevelDBWriter
import com.zbsnetwork.db.LevelDBFactory
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.mining.MiningConstraint
import com.zbsnetwork.settings.FunctionalitySettings
import com.zbsnetwork.state.diffs.BlockDiffer
import com.zbsnetwork.transaction.{GenesisTransaction, Transaction}
import monix.execution.UncaughtExceptionReporter
import monix.reactive.Observer
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations.{Setup, TearDown}
import org.scalacheck.{Arbitrary, Gen}

trait BaseState {
  import BaseState._

  private val fsSettings: FunctionalitySettings = updateFunctionalitySettings(FunctionalitySettings.TESTNET)
  private val db: DB = {
    val dir     = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
    val options = new Options()
    options.createIfMissing(true)
    LevelDBFactory.factory.open(new File(dir), options)
  }

  private val portfolioChanges = Observer.empty(UncaughtExceptionReporter.LogExceptionsToStandardErr)
  val state: LevelDBWriter     = new LevelDBWriter(db, portfolioChanges, fsSettings, 100000, 2000, 120 * 60 * 1000)

  private var _richAccount: PrivateKeyAccount = _
  def richAccount: PrivateKeyAccount          = _richAccount

  private var _lastBlock: Block = _
  def lastBlock: Block          = _lastBlock

  protected def zbs(n: Float): Long              = (n * 100000000L).toLong
  protected val accountGen: Gen[PrivateKeyAccount] = Gen.containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte]).map(seed => PrivateKeyAccount(seed))

  protected def updateFunctionalitySettings(base: FunctionalitySettings): FunctionalitySettings = base

  protected def txGenP(sender: PrivateKeyAccount, ts: Long): Gen[Transaction]

  private def genBlock(base: Block, sender: PrivateKeyAccount): Gen[Block] =
    for {
      transferTxs <- Gen.sequence[Vector[Transaction], Transaction]((1 to TxsInBlock).map { i =>
        txGenP(sender, base.timestamp + i)
      })
    } yield
      TestBlock.create(
        time = transferTxs.last.timestamp,
        ref = base.uniqueId,
        txs = transferTxs
      )

  private val initGen: Gen[(PrivateKeyAccount, Block)] = for {
    rich <- accountGen
  } yield {
    val genesisTx = GenesisTransaction.create(rich, zbs(100000000L), System.currentTimeMillis() - 10000).explicitGet()
    (rich, TestBlock.create(time = genesisTx.timestamp, Seq(genesisTx)))
  }

  protected def nextBlock(txs: Seq[Transaction]): Block = TestBlock.create(
    time = txs.last.timestamp,
    ref = lastBlock.uniqueId,
    txs = txs
  )

  private def append(prev: Option[Block], next: Block): Unit = {
    val preconditionDiff = BlockDiffer.fromBlock(fsSettings, state, prev, next, MiningConstraint.Unlimited).explicitGet()._1
    state.append(preconditionDiff, 0, next)
  }

  def applyBlock(b: Block): Unit = {
    append(Some(lastBlock), b)
    _lastBlock = b
  }

  def genAndApplyNextBlock(): Unit = {
    val b = genBlock(lastBlock, richAccount).sample.get
    applyBlock(b)
  }

  @Setup
  def init(): Unit = {
    val (richAccount, genesisBlock) = initGen.sample.get
    _richAccount = richAccount

    append(None, genesisBlock)
    _lastBlock = genesisBlock
  }

  @TearDown
  def close(): Unit = {
    db.close()
  }
}

object BaseState {
  private val TxsInBlock = 5000
}
