package com.zbsnetwork.it.sync.smartcontract

import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync.{minFee, setScriptFee}
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.util._
import com.zbsnetwork.lang.v1.FunctionHeader
import com.zbsnetwork.lang.v1.compiler.Terms.{CONST_LONG, FUNCTION_CALL}
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.smart.script.ScriptCompiler
import com.zbsnetwork.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.zbsnetwork.transaction.transfer._
import org.scalatest.{CancelAfterFailure, Ignore}
import play.api.libs.json.{JsNumber, Json}

@Ignore // ignored in v0.16
class HodlContractTransactionSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val contract = pkByAddress(firstAddress)
  private val caller   = pkByAddress(secondAddress)

  test("setup contract account with zbs") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = None,
          sender = sender.privateKey,
          recipient = contract,
          amount = 5.zbs,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("setup caller account with zbs") {
    val tx =
      TransferTransactionV2
        .selfSigned(
          assetId = None,
          sender = sender.privateKey,
          recipient = caller,
          amount = 10.zbs,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("set contract to contract account") {
    val scriptText =
      """
        |
        |	@Callable(i)
        |	func deposit() = {
        |   let pmt = extract(i.payment)
        |   if (isDefined(pmt.asset)) then throw("can hodl zbs only at the moment")
        |   else {
        |	  	let currentKey = toBase58String(i.caller.bytes)
        |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
        |	  		case a:Int => a
        |	  		case _ => 0
        |	  	}
        |	  	let newAmount = currentAmount + pmt.amount
        |	  	WriteSet(List(DataEntry(currentKey, newAmount)))
        |
        |   }
        |	}
        |
        | @Callable(i)
        | func withdraw(amount: Int) = {
        |	  	let currentKey = toBase58String(i.caller.bytes)
        |	  	let currentAmount = match getInteger(i.contractAddress, currentKey) {
        |	  		case a:Int => a
        |	  		case _ => 0
        |	  	}
        |		let newAmount = currentAmount - amount
        |	 if (amount < 0)
        |			then throw("Can't withdraw negative amount")
        |  else if (newAmount < 0)
        |			then throw("Not enough balance")
        |			else ContractResult(
        |					WriteSet(List(DataEntry(currentKey, newAmount))),
        |					TransferSet(List(ContractTransfer(i.caller, amount, unit)))
        |				)
        |	}
        |
        |
        |
        """.stripMargin

    val script = ScriptCompiler.contract(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(contract, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val acc0ScriptInfo = sender.addressScriptInfo(contract.address)

    acc0ScriptInfo.script.isEmpty shouldBe false
    acc0ScriptInfo.scriptText.isEmpty shouldBe false
    acc0ScriptInfo.script.get.startsWith("base64:") shouldBe true

    val json = Json.parse(sender.get(s"/transactions/info/$setScriptId").getResponseBody)
    (json \ "script").as[String].startsWith("base64:") shouldBe true
  }

  test("caller deposits zbs") {
    val balanceBefore = sender.accountBalances(contract.address)._1
    val tx =
      ContractInvocationTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("deposit"), List.empty),
          p = Some(ContractInvocationTransaction.Payment(1.5.zbs, None)),
          timestamp = System.currentTimeMillis(),
          fee = 1.zbs
        )
        .explicitGet()

    val contractInvocationId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(ContractInvocationTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(contractInvocationId)

    sender.getData(contract.address, caller.address) shouldBe IntegerDataEntry(caller.address, 1.5.zbs)
    val balanceAfter = sender.accountBalances(contract.address)._1

    (balanceAfter - balanceBefore) shouldBe 1.5.zbs
  }

  test("caller can't withdraw more than owns") {
    val tx =
      ContractInvocationTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("withdraw"), List(CONST_LONG(1.51.zbs))),
          p = None,
          timestamp = System.currentTimeMillis(),
          fee = 1.zbs
        )
        .explicitGet()

    assertBadRequestAndMessage(sender
                                 .signedBroadcast(tx.json() + ("type" -> JsNumber(ContractInvocationTransaction.typeId.toInt))),
                               "Not enough balance")
  }

  test("caller can withdraw less than he owns") {
    val balanceBefore = sender.accountBalances(contract.address)._1
    val tx =
      ContractInvocationTransaction
        .selfSigned(
          sender = caller,
          contractAddress = contract,
          fc = FUNCTION_CALL(FunctionHeader.User("withdraw"), List(CONST_LONG(1.49.zbs))),
          p = None,
          timestamp = System.currentTimeMillis(),
          fee = 1.zbs
        )
        .explicitGet()

    val contractInvocationId = sender
      .signedBroadcast(tx.json() + ("type" -> JsNumber(ContractInvocationTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(contractInvocationId)

    val balanceAfter = sender.accountBalances(contract.address)._1

    sender.getData(contract.address, caller.address) shouldBe IntegerDataEntry(caller.address, 0.01.zbs)
    (balanceAfter - balanceBefore) shouldBe -1.49.zbs
  }
}
