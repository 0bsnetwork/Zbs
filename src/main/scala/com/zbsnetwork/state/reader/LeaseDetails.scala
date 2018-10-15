package com.zbsplatform.state.reader

import com.zbsplatform.account.{AddressOrAlias, PublicKeyAccount}

case class LeaseDetails(sender: PublicKeyAccount, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
