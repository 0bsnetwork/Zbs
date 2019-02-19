package com.zbsnetwork.it.matcher

import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.it.Node
import com.zbsnetwork.transaction.assets.exchange.Order

sealed trait MatcherCommand extends Product with Serializable
object MatcherCommand {
  case class Place(node: Node, order: Order)                            extends MatcherCommand
  case class Cancel(node: Node, owner: PrivateKeyAccount, order: Order) extends MatcherCommand
}
