package com.zbsnetwork.state

import com.zbsnetwork.block.Block.BlockId
import com.zbsnetwork.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
