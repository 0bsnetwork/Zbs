package com.zbsnetwork

import com.zbsnetwork.utils.base58Length
import com.zbsnetwork.block.{Block, MicroBlock}
import com.zbsnetwork.common.state.ByteStr

package object transaction {

  type AssetId = ByteStr
  val AssetIdLength: Int       = com.zbsnetwork.crypto.DigestSize
  val AssetIdStringLength: Int = base58Length(AssetIdLength)
  type DiscardedTransactions = Seq[Transaction]
  type DiscardedBlocks       = Seq[Block]
  type DiscardedMicroBlocks  = Seq[MicroBlock]
  type AuthorizedTransaction = Authorized with Transaction
}
