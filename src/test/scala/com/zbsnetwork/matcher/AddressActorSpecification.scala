package com.zbsnetwork.matcher

import java.util.concurrent.atomic.AtomicReference

import akka.actor.{ActorRef, ActorSystem, PoisonPill, Props}
import akka.testkit.{ImplicitSender, TestKit, TestProbe}
import cats.kernel.Monoid
import com.zbsnetwork.NTPTime
import com.zbsnetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.matcher.AddressActor.{BalanceUpdated, PlaceOrder}
import com.zbsnetwork.matcher.model.LimitOrder
import com.zbsnetwork.matcher.queue.{QueueEvent, QueueEventWithMeta}
import com.zbsnetwork.state.{LeaseBalance, Portfolio}
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order, OrderType, OrderV1}
import com.zbsnetwork.wallet.Wallet
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class AddressActorSpecification
    extends TestKit(ActorSystem("AddressActorSpecification"))
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll
    with ImplicitSender
    with NTPTime {

  private val assetId    = ByteStr("asset".getBytes)
  private val matcherFee = 30000L

  private val sellTokenOrder1 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKeyAccount("matcher".getBytes()),
    pair = AssetPair(None, Some(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 1L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken1Portfolio = requiredPortfolio(sellTokenOrder1)

  private val sellTokenOrder2 = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKeyAccount("matcher".getBytes()),
    pair = AssetPair(None, Some(assetId)),
    orderType = OrderType.BUY,
    price = 100000000L,
    amount = 100L,
    timestamp = 2L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellToken2Portfolio = requiredPortfolio(sellTokenOrder2)

  private val sellZbsOrder = OrderV1(
    sender = privateKey("test"),
    matcher = PublicKeyAccount("matcher".getBytes()),
    pair = AssetPair(None, Some(assetId)),
    orderType = OrderType.SELL,
    price = 100000000L,
    amount = 100L,
    timestamp = 3L,
    expiration = 1000L,
    matcherFee = matcherFee
  )

  private val sellZbsPortfolio = requiredPortfolio(sellZbsOrder)

  "AddressActorSpecification" should {
    "cancel orders" when {
      "asset balance changed" in test { (ref, eventsProbe, updatePortfolio) =>
        val initPortfolio = sellToken1Portfolio
        updatePortfolio(initPortfolio, false)

        ref ! PlaceOrder(sellTokenOrder1)
        eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

        updatePortfolio(initPortfolio.copy(assets = Map.empty), true)
        eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      }

      "zbs balance changed" when {
        "there are zbs for fee" in zbsBalanceTest(restZbs = matcherFee)
        "there are no zbs at all" in zbsBalanceTest(restZbs = 0L)

        def zbsBalanceTest(restZbs: Long): Unit = test { (ref, eventsProbe, updatePortfolio) =>
          val initPortfolio = sellZbsPortfolio
          updatePortfolio(initPortfolio, false)

          ref ! PlaceOrder(sellZbsOrder)
          eventsProbe.expectMsg(QueueEvent.Placed(sellZbsOrder))

          updatePortfolio(initPortfolio.copy(balance = restZbs), true)
          eventsProbe.expectMsg(QueueEvent.Canceled(sellZbsOrder.assetPair, sellZbsOrder.id()))
        }
      }

      "zbs were leased" when {
        "there are zbs for fee" in leaseTest(_ => matcherFee)
        "there are no zbs at all" in leaseTest(_.spendableBalance)

        def leaseTest(leasedZbs: Portfolio => Long): Unit = test { (ref, eventsProbe, updatePortfolio) =>
          val initPortfolio = sellZbsPortfolio
          updatePortfolio(initPortfolio, false)

          ref ! PlaceOrder(sellZbsOrder)
          eventsProbe.expectMsg(QueueEvent.Placed(sellZbsOrder))

          updatePortfolio(initPortfolio.copy(lease = LeaseBalance(0, leasedZbs(initPortfolio))), true)
          eventsProbe.expectMsg(QueueEvent.Canceled(sellZbsOrder.assetPair, sellZbsOrder.id()))
        }
      }
    }

    "track canceled orders and don't cancel more on same BalanceUpdated message" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combine(sellToken1Portfolio, sellToken2Portfolio)
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellToken1Portfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))

      updatePortfolio(sellToken1Portfolio, true) // same event
      eventsProbe.expectNoMessage()
    }

    "cancel multiple orders" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellZbsPortfolio))
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellZbsPortfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "cancel only orders, those aren't fit" in test { (ref, eventsProbe, updatePortfolio) =>
      val initPortfolio = Monoid.combineAll(Seq(sellToken1Portfolio, sellToken2Portfolio, sellZbsPortfolio))
      updatePortfolio(initPortfolio, false)

      ref ! PlaceOrder(sellTokenOrder1)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder1))

      ref ! PlaceOrder(sellZbsOrder)
      eventsProbe.expectMsg(QueueEvent.Placed(sellZbsOrder))

      ref ! PlaceOrder(sellTokenOrder2)
      eventsProbe.expectMsg(QueueEvent.Placed(sellTokenOrder2))

      updatePortfolio(sellZbsPortfolio, true)
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder1.assetPair, sellTokenOrder1.id()))
      eventsProbe.expectMsg(QueueEvent.Canceled(sellTokenOrder2.assetPair, sellTokenOrder2.id()))
    }

    "schedule expired order cancellation" in {
      pending
    }
  }

  /**
    * (updatedPortfolio: Portfolio, sendBalanceChanged: Boolean) => Unit
    */
  private def test(f: (ActorRef, TestProbe, (Portfolio, Boolean) => Unit) => Unit): Unit = {
    val eventsProbe      = TestProbe()
    val currentPortfolio = new AtomicReference[Portfolio]()
    val address          = addr("test")
    val addressActor = system.actorOf(
      Props(
        new AddressActor(
          address,
          x => currentPortfolio.get().spendableBalanceOf(x),
          1.day,
          ntpTime,
          EmptyOrderDB,
          _ => false,
          event => {
            eventsProbe.ref ! event
            Future.successful(QueueEventWithMeta(0, 0, event))
          }
        )))
    f(
      addressActor,
      eventsProbe,
      (updatedPortfolio, notify) => {
        val prevPortfolio = currentPortfolio.getAndSet(updatedPortfolio)
        if (notify) addressActor ! BalanceUpdated(prevPortfolio.changedAssetIds(updatedPortfolio))
      }
    )
    addressActor ! PoisonPill
  }

  private def requiredPortfolio(order: Order): Portfolio = {
    val b = LimitOrder(order).requiredBalance
    Portfolio(b.getOrElse(None, 0L), LeaseBalance.empty, b.collect { case (Some(id), v) => id -> v })
  }

  private def addr(seed: String): Address                 = privateKey(seed).toAddress
  private def privateKey(seed: String): PrivateKeyAccount = Wallet.generateNewAccount(seed.getBytes(), 0)

  override protected def afterAll(): Unit = {
    TestKit.shutdownActorSystem(system)
    super.afterAll()
  }
}
