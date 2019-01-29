package com.zbsnetwork.it.sync.smartcontract

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync._
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.smart.script.ScriptCompiler
import com.zbsnetwork.transaction.transfer.TransferTransactionV2
import org.scalatest.CancelAfterFailure

class RIDEFuncSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)

  test("assetBalance() verification") {
    val asset = sender
      .issue(acc0.address, "SomeCoin", "SomeDescription", someAssetAmount, 0, reissuable = false, issueFee, 2, waitForTx = true)
      .id

    val newAddress   = sender.createAddress()
    val pkNewAddress = pkByAddress(newAddress)

    sender.transfer(acc0.address, newAddress, 10.zbs, minFee, waitForTx = true)

    val scriptSrc =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') > 0
         |}
      """.stripMargin

    val compiled = ScriptCompiler(scriptSrc, isAssetScript = false).explicitGet()._1

    val tx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(1, pkNewAddress, Some(compiled), setScriptFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(tx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(2, None, pkNewAddress, pkNewAddress, 1.zbs, System.currentTimeMillis(), None, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(2, Some(ByteStr.decodeBase58(asset).get), acc0, pkNewAddress, 100000000, System.currentTimeMillis(), None, smartMinFee, Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transfer = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(2, None, pkNewAddress, pkNewAddress, 1.zbs, System.currentTimeMillis(), None, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transfer.id)

    val udpatedScript =
      s"""
         |match tx {
         |  case tx : SetScriptTransaction => true
         |  case other => assetBalance(tx.sender, base58'$asset') >= 900000000 && zbsBalance(tx.sender) >500000000
         |}
      """.stripMargin

    val updated = ScriptCompiler(udpatedScript, isAssetScript = false).explicitGet()._1

    val updTx =
      sender.signedBroadcast(
        SetScriptTransaction.selfSigned(1, pkNewAddress, Some(updated), setScriptFee + smartFee, System.currentTimeMillis()).explicitGet().json())
    nodes.waitForHeightAriseAndTxPresent(updTx.id)

    assertBadRequestAndResponse(
      sender.signedBroadcast(
        TransferTransactionV2
          .selfSigned(2, None, pkNewAddress, pkNewAddress, 1.zbs, System.currentTimeMillis(), None, smartMinFee, Array())
          .explicitGet()
          .json()),
      "Transaction is not allowed by account-script"
    )

    sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(2, Some(ByteStr.decodeBase58(asset).get), acc0, pkNewAddress, 800000000, System.currentTimeMillis(), None, smartMinFee, Array())
        .explicitGet()
        .json(),
      waitForTx = true
    )

    val transferAfterUpd = sender.signedBroadcast(
      TransferTransactionV2
        .selfSigned(2, None, pkNewAddress, pkNewAddress, 1.zbs, System.currentTimeMillis(), None, smartMinFee, Array())
        .explicitGet()
        .json())
    nodes.waitForHeightAriseAndTxPresent(transferAfterUpd.id)
  }
}
