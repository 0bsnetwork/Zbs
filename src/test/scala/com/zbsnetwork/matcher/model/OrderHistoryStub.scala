package com.zbsnetwork.matcher.model

import akka.actor.{ActorRef, ActorSystem, Props}
import com.zbsnetwork.account.Address
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.matcher.queue.QueueEventWithMeta
import com.zbsnetwork.matcher.{AddressActor, TestOrderDB}
import com.zbsnetwork.state.Portfolio
import com.zbsnetwork.utils.Time

import scala.collection.mutable
import scala.concurrent.Future
import scala.concurrent.duration._

class OrderHistoryStub(system: ActorSystem, time: Time) {
  private val refs   = mutable.AnyRefMap.empty[Address, ActorRef]
  private val orders = mutable.AnyRefMap.empty[ByteStr, Address]

  private def actorFor(lo: LimitOrder): ActorRef =
    refs.getOrElseUpdate(
      lo.order.sender,
      system.actorOf(
        Props(
          new AddressActor(
            lo.order.sender,
            Portfolio.empty,
            5.seconds,
            5.seconds,
            time,
            new TestOrderDB(100),
            e => Future.successful(QueueEventWithMeta(0, 0, e)),
          )))
    )

  def ref(sender: Address): ActorRef  = refs(sender)
  def ref(orderId: ByteStr): ActorRef = refs(orders(orderId))

  def process(event: Events.Event): Unit = event match {
    case oa: Events.OrderAdded =>
      orders += oa.order.order.id() -> oa.order.order.sender
      actorFor(oa.order) ! oa

    case ox: Events.OrderExecuted =>
      orders += ox.submitted.order.id() -> ox.submitted.order.sender
      orders += ox.counter.order.id()   -> ox.counter.order.sender
      actorFor(ox.counter) ! ox
      actorFor(ox.submitted) ! ox

    case oc: Events.OrderCanceled =>
      actorFor(oc.limitOrder) ! oc
  }

  def processAll(events: Events.Event*): Unit = events.foreach(process)
}
