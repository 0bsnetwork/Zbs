package com.zbsnetwork.history

import com.zbsnetwork.account.Address
import com.zbsnetwork.block.Block
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.BlockchainUpdater

case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long          = blockchainUpdater.effectiveBalance(a, blockchainUpdater.height, 1000)
  def appendBlock(b: Block)                 = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr)         = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                           = blockchainUpdater.lastBlockId.get
  def portfolio(address: Address)           = blockchainUpdater.portfolio(address)
  def addressTransactions(address: Address) = blockchainUpdater.addressTransactions(address, Set.empty, 128, None)
  def carryFee                              = blockchainUpdater.carryFee
}
