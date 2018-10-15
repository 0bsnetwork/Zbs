package com.zbsplatform.it

import com.zbsplatform.state.DataEntry
import com.zbsplatform.it.util._

package object sync {
  val minFee                     = 0.001.zbs
  val leasingFee                 = 0.002.zbs
  val smartFee                   = 0.004.zbs
  val issueFee                   = 1.zbs
  val burnFee                    = 1.zbs
  val sponsorFee                 = 1.zbs
  val transferAmount             = 10.zbs
  val leasingAmount              = transferAmount
  val issueAmount                = transferAmount
  val massTransferFeePerTransfer = 0.0005.zbs
  val someAssetAmount            = 9999999999999l
  val matcherFee                 = 0.003.zbs

  def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      minFee * (dataSize / 1024 + 1)
    } else minFee
  }

  def calcMassTransferFee(numberOfRecipients: Int): Long = {
    minFee + massTransferFeePerTransfer * (numberOfRecipients + 1)
  }

  val supportedVersions = List(null, "2") //sign and broadcast use default for V1
}
