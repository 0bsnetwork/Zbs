package com.zbsplatform.matcher.market

import akka.actor.{Actor, Props}
import akka.http.scaladsl.model.StatusCodes
import com.zbsplatform.account.Address
import com.zbsplatform.matcher.MatcherSettings
import com.zbsplatform.matcher.api.{MatcherResponse, NotImplemented, OrderDeleted}
import com.zbsplatform.matcher.market.OrderHistoryActor.{ExpirableOrderHistoryRequest, _}
import com.zbsplatform.matcher.model.Events.{OrderAdded, OrderCanceled, OrderExecuted}
import com.zbsplatform.matcher.model._
import com.zbsplatform.metrics.TimerExt
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.AssetAcc
import com.zbsplatform.transaction.ValidationError.GenericError
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order}
import com.zbsplatform.utils.NTP
import com.zbsplatform.utx.UtxPool
import com.zbsplatform.wallet.Wallet
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

class OrderHistoryActor(db: DB, val settings: MatcherSettings, val utxPool: UtxPool, val wallet: Wallet) extends Actor with OrderValidator {

  val orderHistory = new OrderHistory(db, settings)

  private val timer          = Kamon.timer("matcher.order-history")
  private val addedTimer     = timer.refine("event" -> "added")
  private val executedTimer  = timer.refine("event" -> "executed")
  private val cancelledTimer = timer.refine("event" -> "cancelled")

  override def preStart(): Unit = {
    context.system.eventStream.subscribe(self, classOf[OrderAdded])
    context.system.eventStream.subscribe(self, classOf[OrderExecuted])
    context.system.eventStream.subscribe(self, classOf[OrderCanceled])
  }

  def processExpirableRequest(r: Any): Unit = r match {
    case ValidateOrder(o, _) =>
      sender() ! ValidateOrderResult(o.id(), validateNewOrder(o))
    case GetTradableBalance(assetPair, addr, _) =>
      sender() ! getPairTradableBalance(assetPair, addr)
    case DeleteOrderFromHistory(_, address, maybeId, _) =>
      val result = for {
        id <- maybeId.toRight[MatcherResponse](NotImplemented("Batch order deletion is not supported yet"))
        _ <- orderHistory.deleteOrder(address, id).left.map[MatcherResponse] {
          case LimitOrder.NotFound => StatusCodes.NotFound   -> s"Order $id not found"
          case other               => StatusCodes.BadRequest -> s"Invalid status: $other"
        }
      } yield id

      sender() ! result.fold[MatcherResponse](identity, id => OrderDeleted(id))
  }

  override def receive: Receive = {
    case req: ExpirableOrderHistoryRequest =>
      if (NTP.correctedTime() - req.ts < RequestTTL) {
        processExpirableRequest(req)
      }
    case ev: OrderAdded =>
      addedTimer.measure(orderHistory.process(ev))
    case ev: OrderExecuted =>
      executedTimer.measure(orderHistory.process(ev))
    case ev: OrderCanceled =>
      cancelledTimer.measure(orderHistory.process(ev))
    case ForceCancelOrderFromHistory(id) =>
      forceCancelOrder(id)
  }

  def forceCancelOrder(id: ByteStr): Unit = {
    val maybeOrder = orderHistory.order(id)
    for (o <- maybeOrder) {
      val oi = orderHistory.orderInfo(id)
      orderHistory.process(OrderCanceled(LimitOrder.limitOrder(o.price, oi.remaining, oi.remainingFee, o), unmatchable = false))
    }
    sender ! maybeOrder
  }

  def getPairTradableBalance(assetPair: AssetPair, acc: Address): GetTradableBalanceResponse = {
    val bal = (for {
      amountAcc <- Right(AssetAcc(acc, assetPair.amountAsset))
      priceAcc  <- Right(AssetAcc(acc, assetPair.priceAsset))
      amountBal <- Right(getTradableBalance(amountAcc))
      priceBal  <- Right(getTradableBalance(priceAcc))
    } yield (amountBal, priceBal)) match {
      case Left(_)  => (0L, 0L)
      case Right(b) => b
    }

    GetTradableBalanceResponse(
      Map(
        assetPair.amountAssetStr -> bal._1,
        assetPair.priceAssetStr  -> bal._2
      ))
  }
}

object OrderHistoryActor {
  val RequestTTL: Int = 5 * 1000

  def name: String = "OrderHistory"

  def props(db: DB, settings: MatcherSettings, utxPool: UtxPool, wallet: Wallet): Props =
    Props(new OrderHistoryActor(db, settings, utxPool, wallet))

  sealed trait ExpirableOrderHistoryRequest {
    def ts: Long
  }

  case class DeleteOrderFromHistory(assetPair: AssetPair, address: Address, id: Option[ByteStr], ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrder(order: Order, ts: Long) extends ExpirableOrderHistoryRequest

  case class ValidateOrderResult(validatedOrderId: ByteStr, result: Either[GenericError, Order])

  case class ForceCancelOrderFromHistory(orderId: ByteStr)

  case class GetTradableBalance(assetPair: AssetPair, address: Address, ts: Long) extends ExpirableOrderHistoryRequest

  case class GetTradableBalanceResponse(balances: Map[String, Long])
      extends MatcherResponse(StatusCodes.OK, JsObject(balances.map { case (k, v) => (k, JsNumber(v)) }))
}
