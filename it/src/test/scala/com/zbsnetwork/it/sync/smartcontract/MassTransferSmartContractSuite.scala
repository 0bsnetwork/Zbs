package com.zbsplatform.it.sync.smartcontract

import com.zbsplatform.crypto
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.sync._
import com.zbsplatform.it.transactions.BaseTransactionSuite
import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.state._
import com.zbsplatform.transaction.Proofs
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.transfer.MassTransferTransaction.Transfer
import com.zbsplatform.transaction.transfer._
import com.zbsplatform.utils.{Base58, dummyCompilerContext}
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber

import scala.concurrent.duration._

/*
Scenario:
every month a foundation makes payments from two MassTransactions(type == 11):
1) 80% to users
2) 10% as tax and 10% to bank go after 30sec of payment from step 1)
 */

class MassTransferSmartContractSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val fourthAddress: String = sender.createAddress()

  test("airdrop emulation via MassTransfer") {
    val scriptText = {
      val untyped = Parser(s"""
        match tx {
          case ttx: MassTransferTransaction =>
            let commonAmount = (ttx.transfers[0].amount + ttx.transfers[1].amount)
            let totalAmountToUsers = commonAmount == 8000000000
            let totalAmountToGov = commonAmount == 2000000000
            let massTxSize = size(ttx.transfers) == 2

            let accountPK = base58'${ByteStr(sender.publicKey.publicKey)}'
            let accSig = sigVerify(ttx.bodyBytes,ttx.proofs[0],accountPK)

            let txToUsers = (massTxSize && totalAmountToUsers)

            let mTx = transactionById(ttx.proofs[1])

            if (txToUsers && accSig) then true
            else
            if(isDefined(mTx)) then
                match extract(mTx) {
                  case mt2: MassTransferTransaction =>
                    let txToGov = (massTxSize && totalAmountToGov)
                    let txToGovComplete = (ttx.timestamp > mt2.timestamp + 30000) && sigVerify(mt2.bodyBytes,mt2.proofs[0], accountPK)
                    txToGovComplete && accSig && txToGov
                  case _ => false
                }
            else false
        case _ => false
        }
        """.stripMargin).get.value
      CompilerV1(dummyCompilerContext, untyped).explicitGet()._1
    }

    // set script
    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, sender.privateKey, Some(script), minFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    sender.addressScriptInfo(sender.address).scriptText.isEmpty shouldBe false

    //save time
    val currTime = System.currentTimeMillis()

    //make transfer to users
    val transfers =
      MassTransferTransaction
        .parseTransfersList(List(Transfer(thirdAddress, 4 * transferAmount), Transfer(secondAddress, 4 * transferAmount)))
        .right
        .get

    val unsigned =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfers, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()

    val accountSig = ByteStr(crypto.sign(sender.privateKey, unsigned.bodyBytes()))
    val signed     = unsigned.copy(proofs = Proofs(Seq(accountSig)))
    val toUsersID  = sender.signedBroadcast(signed.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(toUsersID)

    //make transfer with incorrect time
    val heightBefore = sender.height

    val transfersToGov =
      MassTransferTransaction.parseTransfersList(List(Transfer(firstAddress, transferAmount), Transfer(fourthAddress, transferAmount))).explicitGet()

    val unsignedToGov =
      MassTransferTransaction
        .create(1, None, sender.publicKey, transfersToGov, currTime, calcMassTransferFee(2) + smartFee, Array.emptyByteArray, Proofs.empty)
        .explicitGet()
    val accountSigToGovFail = ByteStr(crypto.sign(sender.privateKey, unsignedToGov.bodyBytes()))
    val signedToGovFail     = unsignedToGov.copy(proofs = Proofs(Seq(accountSigToGovFail)))

    assertBadRequestAndResponse(sender.signedBroadcast(signedToGovFail.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))),
                                "Transaction not allowed by account-script")

    //make correct transfer to government after some time
    sender.waitForHeight(heightBefore + 10, 2.minutes)

    val unsignedToGovSecond =
      MassTransferTransaction
        .create(1,
                None,
                sender.publicKey,
                transfersToGov,
                System.currentTimeMillis(),
                calcMassTransferFee(2) + smartFee,
                Array.emptyByteArray,
                Proofs.empty)
        .explicitGet()

    val accountSigToGov = ByteStr(crypto.sign(sender.privateKey, unsignedToGovSecond.bodyBytes()))
    val signedToGovGood = unsignedToGovSecond.copy(proofs = Proofs(Seq(accountSigToGov, ByteStr(Base58.decode(toUsersID).get))))
    val massTransferID  = sender.signedBroadcast(signedToGovGood.json() + ("type" -> JsNumber(MassTransferTransaction.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(massTransferID)
  }
}
