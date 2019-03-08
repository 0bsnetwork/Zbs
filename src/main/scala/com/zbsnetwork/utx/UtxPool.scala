package com.zbsnetwork.utx

import com.zbsnetwork.account.Address
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.mining.MultiDimensionalMiningConstraint
import com.zbsnetwork.state.{Diff, Portfolio}
import com.zbsnetwork.transaction._

trait UtxPool extends AutoCloseable {
  self =>

  def putIfNew(tx: Transaction): Either[ValidationError, (Boolean, Diff)]

  def removeAll(txs: Traversable[Transaction]): Unit

  def spendableBalance(addr: Address, assetId: Option[AssetId]): Long

  def pessimisticPortfolio(addr: Address): Portfolio

  def all: Seq[Transaction]

  def size: Int

  def transactionById(transactionId: ByteStr): Option[Transaction]

  def packUnconfirmed(rest: MultiDimensionalMiningConstraint): (Seq[Transaction], MultiDimensionalMiningConstraint)

}
