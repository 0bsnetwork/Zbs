package com.zbsnetwork.matcher.api

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.database.DBExt
import com.zbsnetwork.matcher._
import com.zbsnetwork.transaction.assets.exchange.{ExchangeTransaction, Order}
import org.iq80.leveldb.DB

object DBUtils {
  def order(db: DB, orderId: ByteStr): Option[Order] = db.get(MatcherKeys.order(orderId))

  def transactionsForOrder(db: DB, orderId: ByteStr): Seq[ExchangeTransaction] = db.readOnly { ro =>
    for {
      seqNr <- 1 to ro.get(MatcherKeys.orderTxIdsSeqNr(orderId))
      txId = ro.get(MatcherKeys.orderTxId(orderId, seqNr))
      tx <- ro.get(MatcherKeys.exchangeTransaction(txId))
    } yield tx
  }
}
