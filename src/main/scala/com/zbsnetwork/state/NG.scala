package com.zbsplatform.state

import com.zbsplatform.block.Block.BlockId
import com.zbsplatform.block.MicroBlock

trait NG extends Blockchain {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds: Seq[BlockId]
}
