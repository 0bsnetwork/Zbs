package com.zbsnetwork.transaction

import com.zbsnetwork.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
