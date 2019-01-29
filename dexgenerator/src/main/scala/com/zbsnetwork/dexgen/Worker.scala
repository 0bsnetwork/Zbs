package com.zbsnetwork.dexgen

import java.util.concurrent.ThreadLocalRandom

import cats.Show
import com.zbsnetwork.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.zbsnetwork.api.http.assets.SignedTransferV1Request
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.crypto
import com.zbsnetwork.dexgen.Worker._
import com.zbsnetwork.dexgen.utils.{ApiRequests, GenOrderType}
import com.zbsnetwork.it.api.{MatcherResponse, MatcherStatusResponse, OrderbookHistory, Transaction, UnexpectedStatusCodeException}
import com.zbsnetwork.it.util._
import com.zbsnetwork.matcher.AssetPairBuilder
import com.zbsnetwork.matcher.api.CancelOrderRequest
import com.zbsnetwork.transaction.AssetId
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order}
import com.zbsnetwork.transaction.transfer.TransferTransactionV1
import com.zbsnetwork.utils.LoggerFacade
import org.asynchttpclient.AsyncHttpClient
import org.slf4j.LoggerFactory
import play.api.libs.json._
import settings.{GeneratorSettings, MatcherNodeSettings}

import scala.concurrent._
import scala.concurrent.duration._
import scala.util.{Failure, Success}

class Worker(workerSettings: Settings,
             generatorSettings: GeneratorSettings,
             matcherSettings: MatcherNodeSettings.Settings,
             tradingAssets: Seq[AssetId],
             orderType: GenOrderType.Value,
             ordersCount: Int,
             client: AsyncHttpClient)(implicit ec: ExecutionContext)
    extends ApiRequests(client) {

  import AssetPairCreator._

  log.info("started worker " + orderType)

  def run(): Future[Unit] = placeOrders(workerSettings.iterations)

  private val endpoint: String = generatorSettings.sendTo.head.getHostString

  private val matcherPublicKey = PublicKeyAccount.fromBase58String(matcherSettings.matcherKey).right.get
  private val validAccounts    = generatorSettings.validAccounts
  private val invalidAccounts  = generatorSettings.invalidAccounts
  private val fakeAccounts     = generatorSettings.fakeAccounts

  private val fee = 0.003.zbs

  private def now = System.currentTimeMillis()

  private def Random = ThreadLocalRandom.current()

  private def randomFrom[T](c: Seq[T]): Option[T] = if (c.nonEmpty) Some(c(Random.nextInt(c.size))) else None

  def buyOrder(amount: Long, price: Long, buyer: PrivateKeyAccount, pair: AssetPair)(implicit tag: String): (Order, Future[MatcherResponse]) = {
    to(matcherSettings.endpoint).orderHistory(buyer)
    to(matcherSettings.endpoint).orderBook(pair)
    val order = Order.buy(buyer, matcherPublicKey, pair, amount, price, now, now + 29.day.toMillis, fee)
    log.info(s"[$tag] Buy ${order.id()}: $order")
    val response = to(matcherSettings.endpoint).placeOrder(order).andThen {
      case Failure(e) => log.error(s"[$tag] Can't place buy order ${order.id()}: $e")
    }
    log.info(order.id().base58)
    to(matcherSettings.endpoint).orderHistory(buyer)
    to(matcherSettings.endpoint).orderBook(pair)
    to(matcherSettings.endpoint).orderStatus(order.id().base58, pair)
    (order, response)
  }

  def sellOrder(amount: Long, price: Long, seller: PrivateKeyAccount, pair: AssetPair)(implicit tag: String): (Order, Future[MatcherResponse]) = {
    to(matcherSettings.endpoint).orderHistory(seller)
    to(matcherSettings.endpoint).orderBook(pair)
    val order = Order.sell(seller, matcherPublicKey, pair, amount, price, now, now + 29.day.toMillis, fee)
    log.info(s"[$tag] Sell ${order.id()}: $order")
    val response = to(matcherSettings.endpoint).placeOrder(order).andThen {
      case Failure(e) => log.error(s"[$tag] Can't place sell order ${order.id()}: $e")
    }
    to(matcherSettings.endpoint).orderHistory(seller)
    to(matcherSettings.endpoint).orderBook(pair)
    to(matcherSettings.endpoint).orderStatus(order.id().base58, pair)
    (order, response)
  }

  def cancelOrder(pk: PrivateKeyAccount, pair: AssetPair, orderId: String)(implicit tag: String): Future[MatcherStatusResponse] = {
    log.info(s"[$tag] Cancel $orderId in $pair")
    val request       = CancelOrderRequest(PublicKeyAccount(pk.publicKey), ByteStr.decodeBase58(orderId).toOption, None, Array.emptyByteArray)
    val sig           = crypto.sign(pk, request.toSign)
    val signedRequest = request.copy(signature = sig)
    to(matcherSettings.endpoint).cancelOrder(pair.amountAssetStr, pair.priceAssetStr, signedRequest).andThen {
      case Failure(e) => log.error(s"[$tag] Can't cancel order $orderId: $e")
    }
  }

  def orderHistory(account: PrivateKeyAccount)(implicit tag: String): Future[Seq[OrderbookHistory]] = {
    to(matcherSettings.endpoint).orderHistory(account)
  }

  def cancelAllOrders(fakeAccounts: Seq[PrivateKeyAccount])(implicit tag: String): Future[Seq[MatcherStatusResponse]] = {
    log.info(s"[$tag] Cancel orders of accounts: ${fakeAccounts.map(_.address).mkString(", ")}")
    def cancelOrdersOf(account: PrivateKeyAccount): Future[Seq[MatcherStatusResponse]] = {
      orderHistory(account).flatMap { orders =>
        Future.sequence {
          orders
            .filter(_.isActive)
            .map { order =>
              cancelOrder(account, order.assetPair, order.id)
            }
        }
      }
    }

    Future.sequence(fakeAccounts.map(cancelOrdersOf)).map(_.flatten)
  }

  implicit val signedTransferRequestWrites: Writes[SignedTransferV1Request] =
    Json.writes[SignedTransferV1Request].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(TransferTransactionV1.typeId.toInt)))

  def transfer(sender: PrivateKeyAccount, assetId: Option[AssetId], recipient: PrivateKeyAccount, halfBalance: Boolean)(
      implicit tag: String): Future[Transaction] =
    to(endpoint).balance(sender.address, assetId).flatMap { balance =>
      val halfAmount     = if (halfBalance) balance / 2 else balance
      val transferAmount = assetId.fold(halfAmount - 0.001.zbs)(_ => halfAmount)

      TransferTransactionV1.selfSigned(assetId,
                                       sender,
                                       AddressOrAlias.fromString(PublicKeyAccount(recipient.publicKey).address).right.get,
                                       transferAmount,
                                       now,
                                       None,
                                       fee,
                                       Array.emptyByteArray) match {
        case Left(e) => throw new RuntimeException(s"[$tag] Generated transaction is wrong: $e")
        case Right(txRequest) =>
          log.info(
            s"[$tag] ${assetId.fold("Zbs")(_.base58)} balance of ${sender.address}: $balance, sending $transferAmount to ${recipient.address}")
          val signedTx = createSignedTransferRequest(txRequest)
          to(endpoint).broadcastRequest(signedTx).flatMap { tx =>
            to(endpoint).waitForTransaction(tx.id)
          }
      }
    }

  def send(orderType: GenOrderType.Value): Future[Any] = {
    implicit val tag: String = s"$orderType, ${Random.nextInt(1, 1000000)}"

    val tradingAssetsSize = tradingAssets.size
    val pair = createAssetPair(randomFrom(tradingAssets.dropRight(2).dropRight(tradingAssetsSize / 2)),
                               randomFrom(tradingAssets.dropRight(2).takeRight(tradingAssetsSize / 2 - 1)))

    val work = orderType match {
      case GenOrderType.ActiveBuy =>
        val buyer = randomFrom(validAccounts).get
        buyOrder(DefaultAmount, DefaultPrice - Random.nextInt(2, DefaultPrice / 10), buyer, pair)._2

      case GenOrderType.ActiveSell =>
        val seller = randomFrom(validAccounts).get
        sellOrder(DefaultAmount, DefaultPrice + Random.nextInt(2, DefaultPrice / 5), seller, pair)._2

      case GenOrderType.Buy =>
        val buyer = randomFrom(validAccounts).get
        buyOrder(DefaultAmount, DefaultPrice + Random.nextInt(2, 1000), buyer, pair)._2

      case GenOrderType.Sell =>
        val seller = randomFrom(validAccounts).get
        sellOrder(DefaultAmount, DefaultPrice - Random.nextInt(2, 1000), seller, pair)._2

      case GenOrderType.Cancel =>
        val buyer = randomFrom(validAccounts).get
        sellOrder(DefaultAmount, DefaultPrice * 15, buyer, pair)._2.flatMap { orderInfo =>
          cancelOrder(buyer, pair, orderInfo.message.id)
        }

      case GenOrderType.InvalidAmount =>
        val invalidBuyer = randomFrom(invalidAccounts).get
        val pair         = AssetPair(randomFrom(tradingAssets.takeRight(2)), None)
        buyOrder(DefaultAmount, DefaultPrice, invalidBuyer, pair)._2
          .transformWith {
            case Success(x) => Future.failed(new IllegalStateException(s"Order should not be placed: $x"))
            case Failure(e: UnexpectedStatusCodeException) =>
              if (e.statusCode == 400) Future.successful(())
              else Future.failed(e)
            case Failure(e) => Future.failed(e)
          }

      case GenOrderType.FakeSell =>
        val seller: PrivateKeyAccount = fakeAccounts.head
        val buyer: PrivateKeyAccount  = fakeAccounts(1)
        val pair                      = AssetPair(randomFrom(tradingAssets.takeRight(2)), None)
        for {
          _ <- cancelAllOrders(fakeAccounts)
          _ <- sellOrder(DefaultAmount, DefaultPrice, seller, pair)._2
          _ <- transfer(seller, pair.amountAsset, buyer, halfBalance = false)
          _ <- buyOrder(DefaultAmount, DefaultPrice, buyer, pair)._2
          _ <- transfer(buyer, pair.amountAsset, seller, halfBalance = true)
          _ <- cancelAllOrders(fakeAccounts)
        } yield ()

      case GenOrderType.FakeBuy =>
        val seller: PrivateKeyAccount = fakeAccounts(2)
        val buyer: PrivateKeyAccount  = fakeAccounts(3)
        val pair                      = AssetPair(randomFrom(tradingAssets.takeRight(2)), None)
        for {
          _ <- cancelAllOrders(fakeAccounts)
          _ <- buyOrder(DefaultAmount, DefaultPrice, buyer, pair)._2
          _ <- transfer(buyer, pair.amountAsset, seller, halfBalance = false)
          _ <- sellOrder(DefaultAmount, DefaultPrice, seller, pair)._2
          _ <- transfer(seller, pair.amountAsset, buyer, halfBalance = true)
        } yield ()
    }

    work.andThen {
      case Failure(e) => log.error(s"[$tag] Failed: ${e.getMessage}", e)
    }
  }

  private def serial(times: Int)(f: => Future[Any]): Future[Unit] = {
    def loop(rest: Int, acc: Future[Unit]): Future[Unit] = {
      if (rest <= 0) acc
      else {
        val newAcc = acc.flatMap(_ => f).map(_ => ())
        loop(rest - 1, newAcc)
      }
    }

    loop(times, Future.successful(()))
  }

  private def placeOrders(maxIterations: Int): Future[Unit] = {
    def sendAll(step: Int): Future[Unit] = {
      log.info(s"Step $step")
      serial(ordersCount)(send(orderType)) // @TODO Should work in parallel, but now it leads to invalid transfers
    }

    def runStepsFrom(step: Int): Future[Unit] = sendAll(step).flatMap { _ =>
      val nextStep = step + 1
      if (nextStep < maxIterations) {
        log.info(s"Sleeping ${workerSettings.delay.toMillis}ms")
        GlobalTimer.instance.sleep(workerSettings.delay).flatMap(_ => runStepsFrom(nextStep))
      } else {
        log.info("Done")
        Future.successful(())
      }
    }

    runStepsFrom(1)
  }

  override protected def log: LoggerFacade = LoggerFacade(LoggerFactory.getLogger(toString))
}

object Worker {

  private val DefaultAmount = 100000
  private val DefaultPrice  = 5000000

  case class Settings(autoReconnect: Boolean, iterations: Int, delay: FiniteDuration, reconnectDelay: FiniteDuration)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._

      s"""number of iterations: $iterations
         |delay between iterations: $delay
         |auto reconnect: ${if (autoReconnect) "enabled" else "disabled"}
         |reconnect delay: $reconnectDelay""".stripMargin

    }
  }

}

object AssetPairCreator {
  val ZbsName = "ZBS"

  def createAssetPair(asset1: Option[AssetId], asset2: Option[AssetId]): AssetPair =
    if (AssetPairBuilder.assetIdOrdering.compare(asset1, asset2) > 0)
      AssetPair(asset1, asset2)
    else
      AssetPair(asset2, asset1)
}
