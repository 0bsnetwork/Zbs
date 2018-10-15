package com.zbsplatform

import com.zbsplatform.utils.base58Length
import com.zbsplatform.block.{Block, MicroBlock}

package object transaction {

  type AssetId = com.zbsplatform.state.ByteStr
  val AssetIdLength: Int       = com.zbsplatform.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
