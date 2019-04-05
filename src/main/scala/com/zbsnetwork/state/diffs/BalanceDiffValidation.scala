package com.zbsnetwork.state.diffs

import cats.implicits._
import com.zbsnetwork.account.Address
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.settings.FunctionalitySettings
import com.zbsnetwork.state.{Blockchain, Diff, Portfolio}
import com.zbsnetwork.transaction.ValidationError.AccountBalanceError
import com.zbsnetwork.utils.ScorexLogging

import scala.util.{Left, Right}

object BalanceDiffValidation extends ScorexLogging {

  def apply(b: Blockchain, currentHeight: Int, fs: FunctionalitySettings)(d: Diff): Either[AccountBalanceError, Diff] = {
    val changedAccounts = d.portfolios.keySet

    def check(acc: Address): Option[(Address, String)] = {
      val portfolioDiff = d.portfolios(acc)

      val balance       = portfolioDiff.balance
      lazy val oldZbs   = b.balance(acc, None)
      lazy val oldLease = b.leaseBalance(acc)
      lazy val lease    = cats.Monoid.combine(oldLease, portfolioDiff.lease)
      (if (balance < 0) {
         val newB = oldZbs + balance

         if (newB < 0) {
           Some(acc -> s"negative zbs balance: $acc, old: ${oldZbs}, new: ${newB}")
         } else if (newB < lease.out && currentHeight > fs.allowLeasedBalanceTransferUntilHeight) {
           Some(acc -> (if (newB + lease.in - lease.out < 0) {
                          s"negative effective balance: $acc, old: ${(oldZbs, oldLease)}, new: ${(newB, lease)}"
                        } else if (portfolioDiff.lease.out == 0) {
                          s"$acc trying to spend leased money"
                        } else {
                          s"leased being more than own: $acc, old: ${(oldZbs, oldLease)}, new: ${(newB, lease)}"
                        }))
         } else {
           None
         }
       } else {
         None
       }) orElse (portfolioDiff.assets find {
        case (a, c) =>
          // Tokens it can produce overflow are exist.
          val oldB = b.balance(acc, Some(a))
          val newB = oldB + c
          newB < 0
      } map { _ =>
        acc -> s"negative asset balance: $acc, new portfolio: ${negativeAssetsInfo(b.portfolio(acc).combine(portfolioDiff))}"
      })
    }

    val positiveBalanceErrors: Map[Address, String] = changedAccounts.flatMap(check).toMap

    if (positiveBalanceErrors.isEmpty) {
      Right(d)
    } else {
      Left(AccountBalanceError(positiveBalanceErrors))
    }
  }

  private def negativeAssetsInfo(p: Portfolio): Map[ByteStr, Long] = p.assets.filter(_._2 < 0)
}
