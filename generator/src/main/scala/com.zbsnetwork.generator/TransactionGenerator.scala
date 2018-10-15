package com.zbsplatform.generator

import com.zbsplatform.transaction.Transaction

trait TransactionGenerator extends Iterator[Iterator[Transaction]] {
  override val hasNext = true
}
