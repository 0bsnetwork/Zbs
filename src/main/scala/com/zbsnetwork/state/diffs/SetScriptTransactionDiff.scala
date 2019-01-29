package com.zbsnetwork.state.diffs

import com.zbsnetwork.state.{Blockchain, Diff, LeaseBalance, Portfolio}
import com.zbsnetwork.transaction.ValidationError
import com.zbsnetwork.transaction.smart.SetScriptTransaction

import scala.util.Right

object SetScriptTransactionDiff {
  def apply(blockchain: Blockchain, height: Int)(tx: SetScriptTransaction): Either[ValidationError, Diff] = {
    val scriptOpt = tx.script
    Right(
      Diff(
        height = height,
        tx = tx,
        portfolios = Map(tx.sender.toAddress -> Portfolio(-tx.fee, LeaseBalance.empty, Map.empty)),
        scripts = Map(tx.sender.toAddress    -> scriptOpt)
      ))
  }
}
