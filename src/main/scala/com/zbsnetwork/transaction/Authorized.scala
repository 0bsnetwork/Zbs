package com.zbsnetwork.transaction

import com.zbsnetwork.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
