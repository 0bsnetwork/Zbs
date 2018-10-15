package com.zbsplatform.it.sync.transactions

import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.transactions.BaseTransactionSuite
import com.zbsplatform.it.util._
import com.zbsplatform.it.sync._
import org.scalatest.prop.TableDrivenPropertyChecks

class IssueTransactionV1Suite extends BaseTransactionSuite with TableDrivenPropertyChecks {

  test("asset issue changes issuer's asset balance; issuer's zbs balance is decreased by fee") {
    val assetName        = "myasset"
    val assetDescription = "my asset description"
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    notMiner.assertBalances(firstAddress, balance1 - issueFee, eff1 - issueFee)
    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
  }

  test("Able to create asset with the same name") {
    val assetName        = "myasset1"
    val assetDescription = "my asset description 1"
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val issuedAssetId = sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = false, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

    val issuedAssetIdSameAsset = sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = true, issueFee).id
    nodes.waitForHeightAriseAndTxPresent(issuedAssetIdSameAsset)

    notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)
    notMiner.assertBalances(firstAddress, balance1 - 2 * issueFee, eff1 - 2 * issueFee)
  }

  test("Not able to create asset when insufficient funds") {
    val assetName        = "myasset"
    val assetDescription = "my asset description"
    val eff1             = notMiner.accountBalances(firstAddress)._2
    val bigAssetFee      = eff1 + 1.zbs

    assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, someAssetAmount, 2, reissuable = false, bigAssetFee),
                               "negative zbs balance")
  }

  val invalidAssetValue =
    Table(
      ("assetVal", "decimals", "message"),
      (0l, 2, "negative amount"),
      (1l, 9, "Too big sequences requested"),
      (-1l, 1, "negative amount"),
      (1l, -1, "Too big sequences requested")
    )

  forAll(invalidAssetValue) { (assetVal: Long, decimals: Int, message: String) =>
    test(s"Not able to create asset total token='$assetVal', decimals='$decimals' ") {
      val assetName          = "myasset2"
      val assetDescription   = "my asset description 2"
      val decimalBytes: Byte = decimals.toByte
      assertBadRequestAndMessage(sender.issue(firstAddress, assetName, assetDescription, assetVal, decimalBytes, reissuable = false, issueFee),
                                 message)
    }
  }

}
