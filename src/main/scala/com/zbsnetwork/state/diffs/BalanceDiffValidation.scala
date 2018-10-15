package com.zbsplatform.state.diffs

import cats.implicits._
import com.zbsplatform.metrics.Instrumented
import com.zbsplatform.settings.FunctionalitySettings
import com.zbsplatform.state.{Blockchain, ByteStr, Diff, LeaseBalance, Portfolio}
import com.zbsplatform.account.Address
import com.zbsplatform.utils.ScorexLogging
import com.zbsplatform.transaction.ValidationError.AccountBalanceError

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging with Instrumented {

  def apply(b: Blockchain, currentHeight: Int, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {

    val changedAccounts = d.portfolios.keySet

    val positiveBalanceErrors: Map[Address, String] = changedAccounts
      .flatMap(acc => {
        val portfolioDiff = d.portfolios(acc)
        val oldPortfolio  = b.portfolio(acc)

        val newPortfolio = oldPortfolio.combine(portfolioDiff)

        lazy val negativeBalance          = newPortfolio.balance < 0
        lazy val negativeAssetBalance     = newPortfolio.assets.values.exists(_ < 0)
        lazy val negativeEffectiveBalance = newPortfolio.effectiveBalance < 0
        lazy val leasedMoreThanOwn        = newPortfolio.balance < newPortfolio.lease.out && currentHeight > fs.allowLeasedBalanceTransferUntilHeight

        val err = if (negativeBalance) {
          Some(s"negative zbs balance: $acc, old: ${oldPortfolio.balance}, new: ${newPortfolio.balance}")
        } else if (negativeAssetBalance) {
          Some(s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(newPortfolio)}")
        } else if (negativeEffectiveBalance) {
          Some(s"negative effective balance: $acc, old: ${leaseZbsInfo(oldPortfolio)}, new: ${leaseZbsInfo(newPortfolio)}")
        } else if (leasedMoreThanOwn && oldPortfolio.lease.out == newPortfolio.lease.out) {
          Some(s"$acc trying to spend leased money")
        } else if (leasedMoreThanOwn) {
          Some(s"leased being more than own: $acc, old: ${leaseZbsInfo(oldPortfolio)}, new: ${leaseZbsInfo(newPortfolio)}")
        } else None
        err.map(acc -> _)
      })
      .toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def leaseZbsInfo(p: Portfolio): (Long, LeaseBalance) = (p.balance, p.lease)

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] = p.assets.filter(_._2 < 0)
}
