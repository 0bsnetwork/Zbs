package com.zbsnetwork.transaction

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.crypto
import monix.eval.Coeval

trait FastHashId extends ProvenTransaction {
  val id: Coeval[AssetId] = Coeval.evalOnce(FastHashId.create(this.bodyBytes()))
}

object FastHashId {
  def create(bodyBytes: Array[Byte]): AssetId = {
    ByteStr(crypto.fastHash(bodyBytes))
  }
}
