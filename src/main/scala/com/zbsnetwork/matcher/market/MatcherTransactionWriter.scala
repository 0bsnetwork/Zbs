package com.zbsplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.zbsplatform.database.{DBExt, RW}
import com.zbsplatform.matcher.api.MatcherResponse
import com.zbsplatform.matcher.model.Events._
import com.zbsplatform.matcher.{MatcherKeys, MatcherSettings}
import com.zbsplatform.state._
import com.zbsplatform.transaction.assets.exchange.ExchangeTransaction
import com.zbsplatform.utils.ScorexLogging
import org.iq80.leveldb.DB
import play.api.libs.json.JsArray

class MatcherTransactionWriter(db: DB) extends Actor with ScorexLogging {

  import MatcherTransactionWriter._

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[ExchangeTransactionCreated])
  }

  override def receive: Receive = {
    case ExchangeTransactionCreated(tx) =>
      saveExchangeTx(tx)
    case GetTransactionsByOrder(orderId) =>
      fetchTransactionsByOrder(orderId)
  }

  def fetchTransactionsByOrder(orderId: ByteStr): Unit = {
    log.trace(s"Loading transactions for order $orderId")
    val txs: Seq[ExchangeTransaction] = db.readOnly { ro =>
      for {
        seqNr <- 1 to ro.get(MatcherKeys.orderTxIdsSeqNr(orderId))
        txId = ro.get(MatcherKeys.orderTxId(orderId, seqNr))
        tx <- ro.get(MatcherKeys.exchangeTransaction(txId))
      } yield tx
    }

    sender() ! GetTransactionsResponse(txs)
  }

  private def saveExchangeTx(tx: ExchangeTransaction): Unit = db.readWrite { rw =>
    log.trace(s"Appending ${tx.id()} to orders [${tx.buyOrder.id()}, ${tx.sellOrder.id()}]")
    rw.put(MatcherKeys.exchangeTransaction(tx.id()), Some(tx))
    appendTxId(rw, tx.buyOrder.id(), tx.id())
    appendTxId(rw, tx.sellOrder.id(), tx.id())
  }
}

object MatcherTransactionWriter {

  def name: String = "MatcherTransactionWriter"

  def props(db: DB, settings: MatcherSettings): Props = Props(new MatcherTransactionWriter(db))

  case class GetTransactionsByOrder(orderId: ByteStr)

  case class GetTransactionsResponse(txs: Seq[ExchangeTransaction]) extends MatcherResponse(StatusCodes.OK, JsArray(txs.map(_.json())))

  private def appendTxId(rw: RW, orderId: ByteStr, txId: ByteStr): Unit = {
    val key       = MatcherKeys.orderTxIdsSeqNr(orderId)
    val nextSeqNr = rw.get(key) + 1
    rw.put(key, nextSeqNr)
    rw.put(MatcherKeys.orderTxId(orderId, nextSeqNr), txId)
  }
}
