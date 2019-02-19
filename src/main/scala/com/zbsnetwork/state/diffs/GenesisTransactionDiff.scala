package com.zbsnetwork.state.diffs

import com.zbsnetwork.state.{Diff, LeaseBalance, Portfolio}
import com.zbsnetwork.transaction.ValidationError.GenericError
import com.zbsnetwork.transaction.{GenesisTransaction, ValidationError}

import scala.util.{Left, Right}

object GenesisTransactionDiff {
  def apply(height: Int)(tx: GenesisTransaction): Either[ValidationError, Diff] = {
    if (height != 1) Left(GenericError("GenesisTransaction cannot appear in non-initial block"))
    else
      Right(Diff(height = height, tx = tx, portfolios = Map(tx.recipient -> Portfolio(balance = tx.amount, LeaseBalance.empty, assets = Map.empty))))
  }
}
