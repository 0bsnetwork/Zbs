package com.zbsnetwork.it.sync.transactions

import cats.implicits._
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync.{issueAmount, issueFee}
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.sync._

class BurnTransactionSuite extends BaseTransactionSuite {

  private val decimals: Byte = 2

  test("burning assets changes issuer's asset balance; issuer's zbs balance is decreased by fee") {
    for (v <- supportedVersions) {
      val (balance, effectiveBalance) = notMiner.accountBalances(firstAddress)
      val issuedAssetId               = sender.issue(firstAddress, s"name+$v", "description", issueAmount, decimals, reissuable = false, fee = issueFee).id

      notMiner.waitForTransaction(issuedAssetId)
      notMiner.assertBalances(firstAddress, balance - issueFee, effectiveBalance - issueFee)
      notMiner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount)
      val details1 = notMiner.assetsDetails(issuedAssetId)
      assert(!details1.reissuable)
      assert(details1.quantity == issueAmount)
      assert(details1.minSponsoredAssetFee.isEmpty)

      // burn half of the coins and check balance
      val burnId = sender.burn(firstAddress, issuedAssetId, issueAmount / 2, minFee, version = v).id

      notMiner.waitForTransaction(burnId)
      notMiner.assertBalances(firstAddress, balance - minFee - issueFee, effectiveBalance - minFee - issueFee)
      notMiner.assertAssetBalance(firstAddress, issuedAssetId, issueAmount / 2)
      val details2 = notMiner.assetsDetails(issuedAssetId)
      assert(!details2.reissuable)
      assert(details2.quantity == issueAmount - issueAmount / 2)

      val assetOpt = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
      assert(assetOpt.exists(_.balance == issueAmount / 2))

      // burn the rest and check again
      val burnIdRest = sender.burn(firstAddress, issuedAssetId, issueAmount / 2, minFee, version = v).id

      notMiner.waitForTransaction(burnIdRest)
      notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)
      val details3 = notMiner.assetsDetails(issuedAssetId)
      assert(!details3.reissuable)
      assert(details3.quantity == 0)
      assert(details1.minSponsoredAssetFee.isEmpty)

      val assetOptRest = notMiner.assetsBalance(firstAddress).balances.find(_.assetId == issuedAssetId)
      assert(assetOptRest.isEmpty)
    }
  }

  test("can burn non-owned asset; issuer asset balance decreased by transfer amount; burner balance decreased by burned amount") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId = sender.issue(firstAddress, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

      notMiner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, minFee, issuedAssetId.some).id

      notMiner.waitForTransaction(transferId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      val burnId = sender.burn(secondAddress, issuedAssetId, transferredQuantity, minFee, v).id

      notMiner.waitForTransaction(burnId)
      sender.assertAssetBalance(secondAddress, issuedAssetId, 0)

      val details = notMiner.assetsDetails(issuedAssetId)
      assert(!details.reissuable)
      assert(details.quantity == issuedQuantity - transferredQuantity)
      assert(details.minSponsoredAssetFee.isEmpty)

      assertBadRequestAndMessage(sender.transfer(secondAddress, firstAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
                                 "Attempt to transfer unavailable funds")
    }
  }

  test("issuer can't burn more tokens than he own") {
    for (v <- supportedVersions) {
      val issuedQuantity = issueAmount
      val burnedQuantity = issuedQuantity * 2

      val issuedAssetId = sender.issue(firstAddress, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

      notMiner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, minFee, v).id, "negative asset balance")
    }
  }

  test("user can't burn more tokens than he own") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2
      val burnedQuantity      = transferredQuantity * 2

      val issuedAssetId = sender.issue(firstAddress, s"name+$v", "description", issuedQuantity, decimals, reissuable = false, issueFee).id

      notMiner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, minFee, issuedAssetId.some).id

      notMiner.waitForTransaction(transferId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity - transferredQuantity)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      assertBadRequestAndMessage(sender.burn(secondAddress, issuedAssetId, burnedQuantity, minFee, v).id, "negative asset balance")
    }
  }

  test("non-owner can burn asset after reissue") {
    for (v <- supportedVersions) {
      val issuedQuantity      = issueAmount
      val transferredQuantity = issuedQuantity / 2

      val issuedAssetId = sender.issue(firstAddress, s"name+$v", "description", issuedQuantity, decimals, reissuable = true, issueFee).id

      notMiner.waitForTransaction(issuedAssetId)
      sender.assertAssetBalance(firstAddress, issuedAssetId, issuedQuantity)

      val transferId = sender.transfer(firstAddress, secondAddress, transferredQuantity, minFee, issuedAssetId.some).id
      notMiner.waitForTransaction(transferId)

      val burnOwnerTxTd = sender.burn(firstAddress, issuedAssetId, transferredQuantity, minFee, v).id
      notMiner.waitForTransaction(burnOwnerTxTd)

      sender.assertAssetBalance(firstAddress, issuedAssetId, 0)
      sender.assertAssetBalance(secondAddress, issuedAssetId, transferredQuantity)

      val details = notMiner.assetsDetails(issuedAssetId)
      assert(details.reissuable)
      assert(details.quantity == transferredQuantity)
      assert(details.minSponsoredAssetFee.isEmpty)

      val reissueId = sender.reissue(firstAddress, issuedAssetId, issuedQuantity, false, issueFee).id
      notMiner.waitForTransaction(reissueId)

      val details1 = notMiner.assetsDetails(issuedAssetId)
      assert(!details1.reissuable)
      assert(details1.quantity == transferredQuantity + issuedQuantity)
      assert(details1.minSponsoredAssetFee.isEmpty)

      val burn1 = sender.burn(firstAddress, issuedAssetId, issuedQuantity, minFee, v).id
      notMiner.waitForTransaction(burn1)

      val burn2 = sender.burn(secondAddress, issuedAssetId, transferredQuantity, minFee, v).id
      notMiner.waitForTransaction(burn2)

      val details2 = notMiner.assetsDetails(issuedAssetId)
      assert(!details2.reissuable)
      assert(details2.quantity == 0)
      assert(details2.minSponsoredAssetFee.isEmpty)

      assertBadRequestAndMessage(sender.reissue(firstAddress, issuedAssetId, issuedQuantity, true, issueFee).id, "Asset is not reissuable")
      assertBadRequestAndMessage(sender.transfer(secondAddress, thirdAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
                                 "Attempt to transfer unavailable funds")
      assertBadRequestAndMessage(sender.transfer(firstAddress, thirdAddress, transferredQuantity / 2, minFee, issuedAssetId.some).id,
                                 "Attempt to transfer unavailable funds")

    }
  }
}
