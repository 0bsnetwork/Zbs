package com.zbsnetwork.transaction
import monix.reactive.Observable
import com.zbsnetwork.block.Block.BlockId
import com.zbsnetwork.block.{Block, MicroBlock}
import com.zbsnetwork.common.state.ByteStr

trait BlockchainUpdater {
  def processBlock(block: Block, verify: Boolean = true): Either[ValidationError, Option[DiscardedTransactions]]

  def processMicroBlock(microBlock: MicroBlock, verify: Boolean = true): Either[ValidationError, Unit]

  def removeAfter(blockId: ByteStr): Either[ValidationError, DiscardedBlocks]

  def lastBlockInfo: Observable[LastBlockInfo]

  def isLastBlockId(id: ByteStr): Boolean

  def shutdown(): Unit
}

case class LastBlockInfo(id: BlockId, height: Int, score: BigInt, ready: Boolean)
