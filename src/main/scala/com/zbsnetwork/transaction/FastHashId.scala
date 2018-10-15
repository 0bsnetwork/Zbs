package com.zbsplatform.transaction

import com.zbsplatform.crypto
import com.zbsplatform.state.ByteStr
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {

  val id: Coeval[AssetId] = Coeval.evalOnce(ByteStr(crypto.fastHash(bodyBytes())))
}
