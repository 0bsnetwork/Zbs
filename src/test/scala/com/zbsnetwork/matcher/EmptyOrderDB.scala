package com.zbsnetwork.matcher

import com.zbsnetwork.account.Address
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.matcher.model.{OrderInfo, OrderStatus}
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order}

object EmptyOrderDB extends OrderDB {
  override def containsInfo(id: ByteStr): Boolean                                                  = false
  override def status(id: ByteStr): OrderStatus.Final                                              = OrderStatus.NotFound
  override def saveOrderInfo(id: ByteStr, sender: Address, oi: OrderInfo[OrderStatus.Final]): Unit = {}
  override def saveOrder(o: Order): Unit                                                           = {}
  override def loadRemainingOrders(owner: Address,
                                   maybePair: Option[AssetPair],
                                   activeOrders: Seq[(ByteStr, OrderInfo[OrderStatus])]): Seq[(ByteStr, OrderInfo[OrderStatus])] =
    Seq.empty
}
