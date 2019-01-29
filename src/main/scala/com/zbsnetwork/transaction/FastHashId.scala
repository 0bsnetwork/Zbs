package com.zbsnetwork.transaction

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.crypto
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
