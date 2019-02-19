package com.zbsnetwork.generator.utils

import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.common.state.ByteStr

object Universe {
  var AccountsWithBalances: List[(PrivateKeyAccount, Long)] = Nil
  var IssuedAssets: List[ByteStr]                           = Nil
  var Leases: List[ByteStr]                                 = Nil
}
