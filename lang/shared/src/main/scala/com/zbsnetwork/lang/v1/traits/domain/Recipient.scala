package com.zbsnetwork.lang.v1.traits.domain

import com.zbsnetwork.common.state.ByteStr

trait Recipient
object Recipient {
  case class Address(bytes: ByteStr) extends Recipient
  case class Alias(name: String)     extends Recipient
}
