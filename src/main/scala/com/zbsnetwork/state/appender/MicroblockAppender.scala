package com.zbsnetwork.state.appender

import cats.data.EitherT
import com.zbsnetwork.block.MicroBlock
import com.zbsnetwork.metrics.{BlockStats, _}
import com.zbsnetwork.network.MicroBlockSynchronizer.MicroblockData
import com.zbsnetwork.network._
import com.zbsnetwork.state.Blockchain
import com.zbsnetwork.transaction.ValidationError.InvalidSignature
import com.zbsnetwork.transaction.{BlockchainUpdater, ValidationError}
import com.zbsnetwork.utils.ScorexLogging
import com.zbsnetwork.utx.UtxPool
import io.netty.channel.Channel
import io.netty.channel.group.ChannelGroup
import kamon.Kamon
import monix.eval.Task
import monix.execution.Scheduler

import scala.util.{Left, Right}

object MicroblockAppender extends ScorexLogging {
  def apply(blockchainUpdater: BlockchainUpdater with Blockchain, utxStorage: UtxPool, scheduler: Scheduler, verify: Boolean = true)(
      microBlock: MicroBlock): Task[Either[ValidationError, Unit]] = {
    def doProcessing() =
      blockchainUpdater
        .processMicroBlock(microBlock, verify)
        .map(_ => utxStorage.removeAll(microBlock.transactionData))

    Task(metrics.microblockProcessingTimeStats.measureSuccessful(doProcessing()))
      .executeOn(scheduler)
  }

  def apply(blockchainUpdater: BlockchainUpdater with Blockchain,
            utxStorage: UtxPool,
            allChannels: ChannelGroup,
            peerDatabase: PeerDatabase,
            scheduler: Scheduler)(ch: Channel, md: MicroblockData): Task[Unit] = {
    import md.microBlock
    val microblockTotalResBlockSig = microBlock.totalResBlockSig
    (for {
      _                <- EitherT(Task.now(microBlock.signaturesValid()))
      validApplication <- EitherT(apply(blockchainUpdater, utxStorage, scheduler)(microBlock))
    } yield validApplication).value.map {
      case Right(()) =>
        md.invOpt match {
          case Some(mi) => allChannels.broadcast(mi, except = md.microblockOwners())
          case None     => log.warn(s"${id(ch)} Not broadcasting MicroBlockInv")
        }
        BlockStats.applied(microBlock)
      case Left(is: InvalidSignature) =>
        peerDatabase.blacklistAndClose(ch, s"Could not append microblock $microblockTotalResBlockSig: $is")
      case Left(ve) =>
        BlockStats.declined(microBlock)
        log.debug(s"${id(ch)} Could not append microblock $microblockTotalResBlockSig: $ve")
    }
  }

  private[this] object metrics {
    val microblockProcessingTimeStats = Kamon.timer("microblock-appender.processing-time")
  }
}
