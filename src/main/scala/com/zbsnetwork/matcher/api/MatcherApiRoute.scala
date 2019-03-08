package com.zbsnetwork.matcher.api

import akka.actor.ActorRef
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.{Directive0, Directive1, Route}
import akka.pattern.ask
import akka.util.Timeout
import com.google.common.primitives.Longs
import com.zbsnetwork.account.{Address, PublicKeyAccount}
import com.zbsnetwork.api.http._
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.Base58
import com.zbsnetwork.crypto
import com.zbsnetwork.matcher.AddressActor.GetOrderStatus
import com.zbsnetwork.matcher.AddressDirectory.{Envelope => Env}
import com.zbsnetwork.matcher.Matcher.StoreEvent
import com.zbsnetwork.matcher.market.MatcherActor.{GetMarkets, GetSnapshotOffsets, MarketData, SnapshotOffsetsResponse}
import com.zbsnetwork.matcher.market.OrderBookActor._
import com.zbsnetwork.matcher.model._
import com.zbsnetwork.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.zbsnetwork.matcher.{AddressActor, AssetPairBuilder, Matcher}
import com.zbsnetwork.metrics.TimerExt
import com.zbsnetwork.settings.{RestAPISettings, ZbsSettings}
import com.zbsnetwork.transaction.AssetId
import com.zbsnetwork.transaction.assets.exchange.OrderJson._
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order}
import com.zbsnetwork.utils.{ScorexLogging, Time}
import io.swagger.annotations._
import javax.ws.rs.Path
import kamon.Kamon
import org.iq80.leveldb.DB
import play.api.libs.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration._
import scala.reflect.ClassTag
import scala.util.Failure

@Path("/matcher")
@Api(value = "/matcher/")
case class MatcherApiRoute(assetPairBuilder: AssetPairBuilder,
                           matcherPublicKey: PublicKeyAccount,
                           matcher: ActorRef,
                           addressActor: ActorRef,
                           storeEvent: StoreEvent,
                           orderBook: AssetPair => Option[Either[Unit, ActorRef]],
                           getMarketStatus: AssetPair => Option[MarketStatus],
                           orderValidator: Order => Either[String, Order],
                           orderBookSnapshot: OrderBookSnapshotHttpCache,
                           zbsSettings: ZbsSettings,
                           matcherStatus: () => Matcher.Status,
                           db: DB,
                           time: Time,
                           currentOffset: () => QueueEventWithMeta.Offset)
    extends ApiRoute
    with ScorexLogging {

  import MatcherApiRoute._
  import PathMatchers._
  import zbsSettings._

  override val settings: RestAPISettings = restAPISettings

  override lazy val route: Route = pathPrefix("matcher") {
    matcherStatusBarrier {
      getMatcherPublicKey
    }
  }

  private def matcherStatusBarrier: Directive0 = matcherStatus() match {
    case Matcher.Status.Working  => pass
    case Matcher.Status.Starting => complete(DuringStart)
    case Matcher.Status.Stopping => complete(DuringShutdown)
  }
  @Path("/")
  @ApiOperation(value = "Matcher Public Key", notes = "Get matcher public key", httpMethod = "GET")
  def getMatcherPublicKey: Route = (pathEndOrSingleSlash & get) {
    complete(JsString(Base58.encode(matcherPublicKey.publicKey)))
  }

}

object MatcherApiRoute {}
