package com.zbsplatform.transaction

import com.zbsplatform.account.Address

case class AssetAcc(account: Address, assetId: Option[AssetId])
