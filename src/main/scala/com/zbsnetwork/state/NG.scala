package com.zbsnetwork.state

import com.zbsnetwork.block.Block.BlockId
import com.zbsnetwork.block.MicroBlock
import com.zbsnetwork.common.state.ByteStr

trait NG extends Blockchain {
  def microBlock(id: ByteStr): Option[MicroBlock]

  def bestLastBlockInfo(maxTimestamp: Long): Option[BlockMinerInfo]

  def lastPersistedBlockIds(count: Int): Seq[BlockId]

  def microblockIds: Seq[BlockId]
}
