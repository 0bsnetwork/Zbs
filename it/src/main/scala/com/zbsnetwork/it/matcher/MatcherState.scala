package com.zbsnetwork.it.matcher

import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.it.api.{MatcherStatusResponse, OrderBookResponse, OrderbookHistory}
import com.zbsnetwork.matcher.queue.QueueEventWithMeta
import com.zbsnetwork.transaction.assets.exchange.AssetPair

case class MatcherState(offset: QueueEventWithMeta.Offset,
                        snapshots: Map[String, QueueEventWithMeta.Offset],
                        orderBooks: Map[AssetPair, OrderBookResponse],
                        orderStatuses: Map[String, MatcherStatusResponse],
                        reservedBalances: Map[PrivateKeyAccount, Map[String, Long]],
                        orderHistory: Map[PrivateKeyAccount, Map[AssetPair, Seq[OrderbookHistory]]])
