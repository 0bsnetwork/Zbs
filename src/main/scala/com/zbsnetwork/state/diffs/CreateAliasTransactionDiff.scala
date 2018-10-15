package com.zbsplatform.state.diffs

import com.zbsplatform.features.BlockchainFeatures
import com.zbsplatform.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.zbsplatform.transaction.ValidationError.GenericError
import com.zbsplatform.transaction.{CreateAliasTransaction, ValidationError}
import com.zbsplatform.features.FeatureProvider._

import scala.util.Right

object CreateAliasTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: CreateAliasTransaction): Either[ValidationError, Diff] =
    if (blockchain.isFeatureActivated(BlockchainFeatures.DataTransaction, height) && !blockchain.canCreateAlias(tx.alias))
      Left(GenericError("Alias already claimed"))
    else
      Right(
        Diff(height = height,
             tx = tx,
             portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
             aliases = Map(tx.alias               -> tx.sender.toAddress)))
}
