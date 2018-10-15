package com.zbsplatform.state

import java.io.File
import java.nio.file.Files

import com.zbsplatform.TransactionGenBase
import com.zbsplatform.database.LevelDBWriter
import com.zbsplatform.db.LevelDBFactory
import com.zbsplatform.mining.MiningConstraint
import com.zbsplatform.settings.FunctionalitySettings
import com.zbsplatform.state.diffs.BlockDiffer
import org.iq80.leveldb.{DB, Options}
import org.openjdk.jmh.annotations.{Setup, TearDown}
import org.scalacheck.Gen
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.block.Block
import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.transaction.{GenesisTransaction, Transaction}

trait BaseState extends TransactionGenBase {
  import BaseState._

  private val fsSettings: FunctionalitySettings = updateFunctionalitySettings(FunctionalitySettings.TESTNET)
  private val db: DB = {
    val dir     = Files.createTempDirectory("state-synthetic").toAbsolutePath.toString
    val options = new Options()
    options.createIfMissing(true)
    LevelDBFactory.factory.open(new File(dir), options)
  }

  val state: LevelDBWriter = new LevelDBWriter(db, fsSettings)

  private var _richAccount: PrivateKeyAccount = _
  def richAccount: PrivateKeyAccount          = _richAccount

  private var _lastBlock: Block = _
  def lastBlock: Block          = _lastBlock

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
  override def close(): Unit = {
    super.close()
    db.close()
  }
}

object BaseState {
  private val TxsInBlock = 5000
}
