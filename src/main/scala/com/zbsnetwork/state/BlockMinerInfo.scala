package com.zbsplatform.state

import com.zbsplatform.block.Block.BlockId
import com.zbsplatform.consensus.nxt.NxtLikeConsensusBlockData

case class BlockMinerInfo(consensus: NxtLikeConsensusBlockData, timestamp: Long, blockId: BlockId)
