package com.zbsplatform

import com.zbsplatform.state.Blockchain
import com.zbsplatform.transaction.Transaction

package object mining {
  private[mining] def createConstConstraint(maxSize: Long, transactionSize: => Long) = OneDimensionalMiningConstraint(
    maxSize,
    new com.zbsplatform.mining.TxEstimators.Fn {
      override def apply(b: Blockchain, t: Transaction) = transactionSize
      override val minEstimate                          = transactionSize
    }
  )
}
