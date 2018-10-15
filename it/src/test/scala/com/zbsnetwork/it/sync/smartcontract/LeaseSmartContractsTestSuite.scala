package com.zbsplatform.it.sync.smartcontract

import com.zbsplatform.crypto
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.sync.{minFee, transferAmount}
import com.zbsplatform.it.transactions.BaseTransactionSuite
import com.zbsplatform.it.util._
import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.state._
import com.zbsplatform.utils.dummyCompilerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import com.zbsplatform.account.{AddressScheme}
import com.zbsplatform.transaction.Proofs
import com.zbsplatform.transaction.lease.{LeaseCancelTransactionV2, LeaseTransactionV2}
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1

class LeaseSmartContractsTestSuite extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  test("set contract, make leasing and cancel leasing") {
    val (balance1, eff1) = notMiner.accountBalances(acc0.address)
    val (balance2, eff2) = notMiner.accountBalances(thirdAddress)

    val txId = sender.transfer(sender.address, acc0.address, 10 * transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    notMiner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = {
      val sc = Parser(s"""
        let pkA = base58'${ByteStr(acc0.publicKey)}'
        let pkB = base58'${ByteStr(acc1.publicKey)}'
        let pkC = base58'${ByteStr(acc2.publicKey)}'

        match tx {
          case ltx: LeaseTransaction => sigVerify(ltx.bodyBytes,ltx.proofs[0],pkA) && sigVerify(ltx.bodyBytes,ltx.proofs[2],pkC)
          case lctx : LeaseCancelTransaction => sigVerify(lctx.bodyBytes,lctx.proofs[1],pkA) && sigVerify(lctx.bodyBytes,lctx.proofs[2],pkB)
          case other => false
        }
        """.stripMargin).get.value
      CompilerV1(dummyCompilerContext, sc).explicitGet()._1
    }

    val script = ScriptV1(scriptText).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(SetScriptTransaction.supportedVersions.head, acc0, Some(script), minFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val unsignedLeasing =
      LeaseTransactionV2
        .create(
          2,
          acc0,
          transferAmount,
          minFee + 0.2.zbs,
          System.currentTimeMillis(),
          acc2,
          Proofs.empty
        )
        .explicitGet()

    val sigLeasingA = ByteStr(crypto.sign(acc0, unsignedLeasing.bodyBytes()))
    val sigLeasingC = ByteStr(crypto.sign(acc2, unsignedLeasing.bodyBytes()))

    val signedLeasing =
      unsignedLeasing.copy(proofs = Proofs(Seq(sigLeasingA, ByteStr.empty, sigLeasingC)))

    val leasingId =
      sender.signedBroadcast(signedLeasing.json() + ("type" -> JsNumber(LeaseTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(leasingId)

    notMiner.assertBalances(firstAddress,
                            balance1 + 10 * transferAmount - (2 * minFee + 0.2.zbs),
                            eff1 + 9 * transferAmount - (2 * minFee + 0.2.zbs))
    notMiner.assertBalances(thirdAddress, balance2, eff2 + transferAmount)

    val unsignedCancelLeasing =
      LeaseCancelTransactionV2
        .create(
          version = 2,
          chainId = AddressScheme.current.chainId,
          sender = acc0,
          leaseId = ByteStr.decodeBase58(leasingId).get,
          fee = minFee + 0.2.zbs,
          timestamp = System.currentTimeMillis(),
          proofs = Proofs.empty
        )
        .explicitGet()

    val sigLeasingCancelA = ByteStr(crypto.sign(acc0, unsignedCancelLeasing.bodyBytes()))
    val sigLeasingCancelB = ByteStr(crypto.sign(acc1, unsignedCancelLeasing.bodyBytes()))

    val signedLeasingCancel =
      unsignedCancelLeasing.copy(proofs = Proofs(Seq(ByteStr.empty, sigLeasingCancelA, sigLeasingCancelB)))

    val leasingCancelId =
      sender.signedBroadcast(signedLeasingCancel.json() + ("type" -> JsNumber(LeaseCancelTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(leasingCancelId)

    notMiner.assertBalances(firstAddress,
                            balance1 + 10 * transferAmount - (3 * minFee + 2 * 0.2.zbs),
                            eff1 + 10 * transferAmount - (3 * minFee + 2 * 0.2.zbs))
    notMiner.assertBalances(thirdAddress, balance2, eff2)

  }
}
