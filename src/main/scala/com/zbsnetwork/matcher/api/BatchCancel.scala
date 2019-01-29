package com.zbsnetwork.matcher.api
import com.zbsnetwork.account.Address
import com.zbsnetwork.transaction.assets.exchange.AssetPair

case class BatchCancel(address: Address, assetPair: Option[AssetPair], timestamp: Long)
