package com.zbsplatform.history

import com.zbsplatform.state._
import com.zbsplatform.account.Address
import com.zbsplatform.block.Block
import com.zbsplatform.transaction.BlockchainUpdater

case class Domain(blockchainUpdater: BlockchainUpdater with NG) {
  def effBalance(a: Address): Long          = blockchainUpdater.effectiveBalance(a, blockchainUpdater.height, 1000)
  def appendBlock(b: Block)                 = blockchainUpdater.processBlock(b).explicitGet()
  def removeAfter(blockId: ByteStr)         = blockchainUpdater.removeAfter(blockId).explicitGet()
  def lastBlockId                           = blockchainUpdater.lastBlockId.get
  def portfolio(address: Address)           = blockchainUpdater.portfolio(address)
  def addressTransactions(address: Address) = blockchainUpdater.addressTransactions(address, Set.empty, 128, 0)
  def carryFee                              = blockchainUpdater.carryFee
}
