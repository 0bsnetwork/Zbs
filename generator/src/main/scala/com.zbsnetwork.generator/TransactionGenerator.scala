package com.zbsnetwork.generator

import com.zbsnetwork.transaction.Transaction

trait TransactionGenerator extends Iterator[Iterator[Transaction]] {
  override val hasNext = true
}
