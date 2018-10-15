package com.zbsplatform.matcher.model

import com.zbsplatform.matcher.MatcherSettings
import com.zbsplatform.matcher.model.Events.OrderExecuted
import com.zbsplatform.settings.FunctionalitySettings
import com.zbsplatform.state.Blockchain
import com.zbsplatform.utils.{NTP, ScorexLogging}
import com.zbsplatform.utx.UtxPool
import com.zbsplatform.transaction.ValidationError
import com.zbsplatform.transaction.assets.exchange.{ExchangeTransaction, Order}
import com.zbsplatform.wallet.Wallet

trait ExchangeTransactionCreator extends ScorexLogging {
  val functionalitySettings: FunctionalitySettings
  val blockchain: Blockchain
  val wallet: Wallet
  val settings: MatcherSettings
  val utx: UtxPool
  private var txTime: Long = 0

  private def getTimestamp: Long = {
    txTime = Math.max(NTP.correctedTime(), txTime + 1)
    txTime
  }

  def createTransaction(event: OrderExecuted): Either[ValidationError, ExchangeTransaction] = {
    import event.{counter, submitted}
    wallet
      .privateKeyAccount(submitted.order.matcherPublicKey)
      .flatMap(matcherPrivateKey => {
        val price             = counter.price
        val (buy, sell)       = Order.splitByType(submitted.order, counter.order)
        val (buyFee, sellFee) = calculateMatcherFee(buy, sell, event.executedAmount)
        ExchangeTransaction.create(matcherPrivateKey, buy, sell, price, event.executedAmount, buyFee, sellFee, settings.orderMatchTxFee, getTimestamp)
      })
  }

  def calculateMatcherFee(buy: Order, sell: Order, amount: Long): (Long, Long) = {
    def calcFee(o: Order, amount: Long): Long = {
      val p = BigInt(amount) * o.matcherFee / o.amount
      p.toLong
    }

    (calcFee(buy, amount), calcFee(sell, amount))
  }
}
