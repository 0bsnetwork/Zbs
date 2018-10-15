package com.zbsplatform.transaction

import com.zbsplatform.account.PublicKeyAccount

trait Authorized {
  val sender: PublicKeyAccount
}
