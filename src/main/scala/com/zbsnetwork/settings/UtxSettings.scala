package com.zbsnetwork.settings

case class UtxSettings(maxSize: Int,
                       maxBytesSize: Long,
                       blacklistSenderAddresses: Set[String],
                       allowBlacklistedTransferTo: Set[String],
                       allowTransactionsFromSmartAccounts: Boolean)
