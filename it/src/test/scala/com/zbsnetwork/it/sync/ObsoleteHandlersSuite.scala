package com.zbsnetwork.it.sync

import com.zbsnetwork.api.http.alias.CreateAliasV1Request
import com.zbsnetwork.api.http.assets.{
  BurnV1Request,
  IssueV1Request,
  MassTransferRequest,
  ReissueV1Request,
  SignedTransferV1Request,
  SponsorFeeRequest,
  TransferV1Request
}
import com.zbsnetwork.api.http.leasing.{LeaseCancelV1Request, LeaseV1Request, SignedLeaseCancelV1Request, SignedLeaseV1Request}
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.Transaction
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.transaction.transfer.TransferTransaction
import com.zbsnetwork.api.http.DataRequest
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.state.{BinaryDataEntry, BooleanDataEntry, IntegerDataEntry, StringDataEntry}
import com.zbsnetwork.transaction.transfer.MassTransferTransaction.Transfer
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import play.api.libs.json.{Json, Writes}

class ObsoleteHandlersSuite extends BaseTransactionSuite {

  test("alias create") {
    val json = sender.postJsonWithApiKey("/alias/create", CreateAliasV1Request(firstAddress, "testalias", minFee))
    val tx   = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets masstransfer") {
    val fee                                     = calcMassTransferFee(2)
    implicit val w: Writes[MassTransferRequest] = Json.writes[MassTransferRequest]
    val transfers                               = List(Transfer(secondAddress, 1.zbs), Transfer(thirdAddress, 2.zbs))
    val json                                    = sender.postJson("/assets/masstransfer", MassTransferRequest(None, firstAddress, transfers, fee, None))
    val tx                                      = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets transfer") {
    val json = sender.postJson("/assets/transfer", TransferV1Request(None, None, transferAmount, minFee, firstAddress, None, secondAddress))
    val tx   = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets issue, burn, reissue, sponsor") {
    val issueJson = sender
      .postJson("/assets/issue", IssueV1Request(firstAddress, "testasset", "testasset", someAssetAmount, 2, true, issueFee))
    val issue = Json.parse(issueJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(issue)

    val burnJson = sender.postJson("/assets/burn", BurnV1Request(firstAddress, issue, someAssetAmount / 2, minFee))
    val burn     = Json.parse(burnJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(burn)

    val reissueJson = sender.postJson("/assets/reissue", ReissueV1Request(firstAddress, issue, someAssetAmount, true, issueFee))
    val reissue     = Json.parse(reissueJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(reissue)

    val sponsorJson = sender.postJson("/assets/sponsor", SponsorFeeRequest(firstAddress, issue, Some(100L), sponsorFee))
    val sponsor     = Json.parse(sponsorJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(sponsor)
  }

  test("leasing lease and cancel") {
    val (balance1, eff1) = notMiner.accountBalances(firstAddress)
    val (balance2, eff2) = notMiner.accountBalances(secondAddress)

    val leaseJson = sender.postJson("/leasing/lease", LeaseV1Request(firstAddress, leasingAmount, minFee, secondAddress))
    val leaseId   = Json.parse(leaseJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseId)

    notMiner.assertBalances(firstAddress, balance1 - minFee, eff1 - leasingAmount - minFee)
    notMiner.assertBalances(secondAddress, balance2, eff2 + leasingAmount)

    val leaseCancelJson = sender.postJson("/leasing/cancel", LeaseCancelV1Request(firstAddress, leaseId, minFee))
    val leaseCancel     = Json.parse(leaseCancelJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseCancel)

    notMiner.assertBalances(firstAddress, balance1 - 2 * minFee, eff1 - 2 * minFee)
    notMiner.assertBalances(secondAddress, balance2, eff2)
  }

  test("addresses data") {
    implicit val w: Writes[DataRequest] = Json.writes[DataRequest]
    val data = List(
      IntegerDataEntry("int", 923275292849183L),
      BooleanDataEntry("bool", value = true),
      BinaryDataEntry("blob", ByteStr(Array.tabulate(445)(_.toByte))),
      StringDataEntry("str", "AAA-AAA")
    )
    val fee  = calcDataFee(data)
    val json = sender.postJson("/addresses/data", DataRequest(firstAddress, data, fee))
    val tx   = Json.parse(json.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(tx)
  }

  test("assets broadcast transfer") {
    val json = Json.obj(
      "type"      -> TransferTransaction.typeId,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> minFee,
      "amount"    -> transferAmount
    )

    val signedRequestResponse = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", json)
    val transfer              = Json.parse(signedRequestResponse.getResponseBody).as[SignedTransferV1Request]

    val transferIdJson = sender.postJson("/assets/broadcast/transfer", transfer)
    val transferId     = Json.parse(transferIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(transferId)
  }

  test("leasing broadcast lease and cancel") {
    val jsonL = Json.obj(
      "type"      -> LeaseTransaction.typeId,
      "sender"    -> firstAddress,
      "recipient" -> secondAddress,
      "fee"       -> minFee,
      "amount"    -> transferAmount
    )

    val r1    = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", jsonL)
    val lease = Json.parse(r1.getResponseBody).as[SignedLeaseV1Request]

    val leaseIdJson = sender.postJson("/leasing/broadcast/lease", lease)
    val leaseId     = Json.parse(leaseIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseId)

    val jsonLC = Json.obj(
      "type"    -> LeaseCancelTransaction.typeId,
      "sender"  -> firstAddress,
      "fee"     -> minFee,
      "leaseId" -> leaseId
    )

    val r2     = sender.postJsonWithApiKey(s"/transactions/sign/$firstAddress", jsonLC)
    val leaseC = Json.parse(r2.getResponseBody).as[SignedLeaseCancelV1Request]

    val leaseCIdJson = sender.postJson("/leasing/broadcast/cancel", leaseC)
    val leaseCId     = Json.parse(leaseCIdJson.getResponseBody).as[Transaction].id
    nodes.waitForTransaction(leaseCId)
  }

}
