package com.zbsnetwork.it.sync.transactions

import com.zbsnetwork.account.AddressOrAlias
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.transfer._
import org.scalatest.CancelAfterFailure

import scala.concurrent.duration._

class TransferTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  test("asset transfer changes sender's and recipient's asset balance; issuer's.zbs balance is decreased by fee") {
    for (v <- supportedVersions) {
      val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

      val issuedAssetId = sender.issue(firstAddress, "name", "description", someAssetAmount, 2, reissuable = false, issueFee).id

      nodes.waitForHeightAriseAndTxPresent(issuedAssetId)

      notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      notMiner.assertAssetBalance(firstAddress, issuedAssetId, someAssetAmount)

      val transferTransactionId = sender.transfer(firstAddress, secondAddress, someAssetAmount, minFee, Some(issuedAssetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(transferTransactionId)

      notMiner.assertBalances(firstAddress, firstBalance - minFee - issueFee, firstEffBalance - minFee - issueFee)
      notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
      notMiner.assertAssetBalance(firstAddress, issuedAssetId, 0)
      notMiner.assertAssetBalance(secondAddress, issuedAssetId, someAssetAmount)
    }
  }

  test("zbs transfer changes zbs balances and eff.b.") {
    for (v <- supportedVersions) {
      val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

      val transferId = sender.transfer(firstAddress, secondAddress, transferAmount, minFee, version = v).id

      nodes.waitForHeightAriseAndTxPresent(transferId)

      notMiner.assertBalances(firstAddress, firstBalance - transferAmount - minFee, firstEffBalance - transferAmount - minFee)
      notMiner.assertBalances(secondAddress, secondBalance + transferAmount, secondEffBalance + transferAmount)
    }
  }

  test("invalid signed zbs transfer should not be in UTX or blockchain") {
    def invalidTx(timestamp: Long = System.currentTimeMillis, fee: Long = 100000): TransferTransactionV1.TransactionT =
      TransferTransactionV1
        .selfSigned(None, sender.privateKey, AddressOrAlias.fromString(sender.address).explicitGet(), 1, timestamp, None, fee, Array.emptyByteArray)
        .right
        .get

    val (balance1, eff1) = notMiner.accountBalances(firstAddress)

    val invalidTxs = Seq(
      (invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis), "Transaction timestamp .* is more than .*ms in the future"),
      (invalidTx(fee = 99999), "Fee .* does not exceed minimal value")
    )

    for ((tx, diag) <- invalidTxs) {
      assertBadRequestAndResponse(sender.broadcastRequest(tx.json()), diag)
      nodes.foreach(_.ensureTxDoesntExist(tx.id().base58))
    }

    nodes.waitForHeightArise()
    notMiner.assertBalances(firstAddress, balance1, eff1)

  }

  test("can not make transfer without having enough effective balance") {
    for (v <- supportedVersions) {
      val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

      assertBadRequest(sender.transfer(secondAddress, firstAddress, secondEffBalance, minFee, version = v))
      nodes.waitForHeightArise()

      notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can not make transfer without having enough balance") {
    for (v <- supportedVersions) {
      val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

      assertBadRequestAndResponse(sender.transfer(secondAddress, firstAddress, secondBalance + 1.zbs, minFee, version = v),
                                  "Attempt to transfer unavailable funds")
      notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }

  test("can forge block with sending majority of some asset to self and to other account") {
    for (v <- supportedVersions) {
      val (firstBalance, firstEffBalance)   = notMiner.accountBalances(firstAddress)
      val (secondBalance, secondEffBalance) = notMiner.accountBalances(secondAddress)

      val assetId = sender.issue(firstAddress, "second asset", "description", someAssetAmount, 0, reissuable = false, fee = issueFee).id

      nodes.waitForHeightAriseAndTxPresent(assetId)

      notMiner.assertBalances(firstAddress, firstBalance - issueFee, firstEffBalance - issueFee)
      notMiner.assertAssetBalance(firstAddress, assetId, someAssetAmount)

      val tx1 = sender.transfer(firstAddress, firstAddress, someAssetAmount, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx1)

      val tx2 = sender.transfer(firstAddress, secondAddress, someAssetAmount / 2, minFee, Some(assetId), version = v).id
      nodes.waitForHeightAriseAndTxPresent(tx2)

      notMiner.assertBalances(firstAddress, firstBalance - issueFee - 2 * minFee, firstEffBalance - issueFee - 2 * minFee)
      notMiner.assertBalances(secondAddress, secondBalance, secondEffBalance)
    }
  }
}
