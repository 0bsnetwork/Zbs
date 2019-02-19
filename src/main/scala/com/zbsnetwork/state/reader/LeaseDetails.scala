package com.zbsnetwork.state.reader

import com.zbsnetwork.account.{AddressOrAlias, PublicKeyAccount}

case class LeaseDetails(sender: PublicKeyAccount, recipient: AddressOrAlias, height: Int, amount: Long, isActive: Boolean)
