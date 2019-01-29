package com.zbsnetwork.mining

import java.security.Permission
import java.util.concurrent.{Semaphore, TimeUnit}

import com.typesafe.config.ConfigFactory
import com.zbsnetwork.account.{Address, PrivateKeyAccount}
import com.zbsnetwork.block.Block
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.consensus.PoSSelector
import com.zbsnetwork.database.LevelDBWriter
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.history.CheckpointServiceImpl
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.mining.BlockWithMaxBaseTargetTest.Env
import com.zbsnetwork.settings.{ZbsSettings, _}
import com.zbsnetwork.state._
import com.zbsnetwork.state.appender.BlockAppender
import com.zbsnetwork.state.diffs.ENOUGH_AMT
import com.zbsnetwork.transaction.{BlockchainUpdater, CheckpointService, GenesisTransaction, Transaction}
import com.zbsnetwork.utils.BaseTargetReachedMaximum
import com.zbsnetwork.utx.UtxPool
import com.zbsnetwork.wallet.Wallet
import com.zbsnetwork.{TransactionGen, WithDB}
import io.netty.channel.group.DefaultChannelGroup
import io.netty.util.concurrent.GlobalEventExecutor
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FreeSpec, Matchers, PrivateMethodTester}

import scala.concurrent.Await
import scala.concurrent.duration._

class BlockWithMaxBaseTargetTest extends FreeSpec with Matchers with WithDB with TransactionGen with PrivateMethodTester {

  "base target limit" - {
    "node should stop if base target greater than maximum in block creation " in {
      withEnv {
        case Env(settings, pos, bcu, checkpoint, utxPoolStub, scheduler, account, lastBlock) =>
          var stopReasonCode = 0

          val allChannels = new DefaultChannelGroup(GlobalEventExecutor.INSTANCE)
          val wallet      = Wallet(WalletSettings(None, Some("123"), None))
          val miner =
            new MinerImpl(allChannels, bcu, checkpoint, settings, ntpTime, utxPoolStub, wallet, pos, scheduler, scheduler)

          val signal = new Semaphore(1)
          signal.acquire()

          System.setSecurityManager(new SecurityManager {
            override def checkPermission(perm: Permission): Unit = {}

            override def checkPermission(perm: Permission, context: Object): Unit = {}

            override def checkExit(status: Int): Unit = signal.synchronized {
              super.checkExit(status)
              stopReasonCode = status
              if (status == BaseTargetReachedMaximum.code)
                signal.release()
              throw new SecurityException("System exit is not allowed")
            }
          })

          val forgeBlock = PrivateMethod[MinerImpl]('forgeBlock)
          miner invokePrivate forgeBlock(account)

          signal.tryAcquire(10, TimeUnit.SECONDS)

          stopReasonCode shouldBe BaseTargetReachedMaximum.code

          System.setSecurityManager(null)
      }
    }

    "node should stop if base target greater than maximum in block append" in {
      withEnv {
        case Env(settings, pos, bcu, checkpoint, utxPoolStub, scheduler, _, lastBlock) =>
          var stopReasonCode = 0

          val signal = new Semaphore(1)
          signal.acquire()

          System.setSecurityManager(new SecurityManager {
            override def checkPermission(perm: Permission): Unit = {}

            override def checkPermission(perm: Permission, context: Object): Unit = {}

            override def checkExit(status: Int): Unit = signal.synchronized {
              super.checkExit(status)
              stopReasonCode = status
              if (status == BaseTargetReachedMaximum.code)
                signal.release()
              throw new SecurityException("System exit is not allowed")
            }
          })

          val blockAppendTask = BlockAppender(checkpoint, bcu, ntpTime, utxPoolStub, pos, settings, scheduler)(lastBlock)
          Await.result(blockAppendTask.runAsync(scheduler), Duration.Inf)

          signal.tryAcquire(10, TimeUnit.SECONDS)

          stopReasonCode shouldBe BaseTargetReachedMaximum.code

          System.setSecurityManager(null)
      }
    }
  }

  def withEnv(f: Env => Unit): Unit = {
    val defaultWriter = new LevelDBWriter(db, TestFunctionalitySettings.Stub, 100000, 2000, 120 * 60 * 1000)

    val settings0     = ZbsSettings.fromConfig(loadConfig(ConfigFactory.load()))
    val minerSettings = settings0.minerSettings.copy(quorum = 0)
    val blockchainSettings0 = settings0.blockchainSettings.copy(
      functionalitySettings = settings0.blockchainSettings.functionalitySettings.copy(
        preActivatedFeatures = Map(BlockchainFeatures.FairPoS.id -> 1)
      )
    )
    val synchronizationSettings0 = settings0.synchronizationSettings.copy(maxBaseTargetOpt = Some(1L))
    val settings = settings0.copy(
      minerSettings = minerSettings,
      featuresSettings = settings0.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false),
      blockchainSettings = blockchainSettings0,
      synchronizationSettings = synchronizationSettings0
    )

    val bcu = new BlockchainUpdaterImpl(defaultWriter, settings, ntpTime)
    val pos = new PoSSelector(bcu, settings.blockchainSettings, settings.synchronizationSettings)

    val checkpoint = new CheckpointServiceImpl(db, settings.checkpointsSettings)
    val utxPoolStub = new UtxPool {
      override def putIfNew(tx: Transaction)                               = ???
      override def removeAll(txs: Traversable[Transaction]): Unit          = {}
      override def accountPortfolio(addr: Address)                         = ???
      override def portfolio(addr: Address)                                = ???
      override def all                                                     = ???
      override def size                                                    = ???
      override def transactionById(transactionId: ByteStr)                 = ???
      override def packUnconfirmed(rest: MultiDimensionalMiningConstraint) = ???
      override def close(): Unit                                           = {}
    }
    val schedulerService: SchedulerService = Scheduler.singleThread("appender")

    try {

      val ts = ntpTime.correctedTime() - 60000
      val (account, firstBlock, secondBlock) =
        Gen
          .containerOfN[Array, Byte](32, Arbitrary.arbitrary[Byte])
          .map(PrivateKeyAccount.apply)
          .map { account =>
            val tx           = GenesisTransaction.create(account, ENOUGH_AMT, ts + 1).explicitGet()
            val genesisBlock = TestBlock.create(ts + 2, List(tx))
            val secondBlock = TestBlock.create(
              ts + 3,
              genesisBlock.uniqueId,
              Seq.empty,
              account
            )
            (account, genesisBlock, secondBlock)
          }
          .sample
          .get

      bcu.processBlock(firstBlock).explicitGet()

      f(Env(settings, pos, bcu, checkpoint, utxPoolStub, schedulerService, account, secondBlock))

      bcu.shutdown()
    } finally {
      bcu.shutdown()
      db.close()
    }
  }
}

object BlockWithMaxBaseTargetTest {

  final case class Env(settings: ZbsSettings,
                       pos: PoSSelector,
                       bcu: BlockchainUpdater with NG,
                       checkpoint: CheckpointService,
                       utxPool: UtxPool,
                       schedulerService: SchedulerService,
                       miner: PrivateKeyAccount,
                       lastBlock: Block)
}
