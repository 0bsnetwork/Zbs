package com.zbsnetwork.it.sync.smartcontract

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.{Base58, EitherExt2}
import com.zbsnetwork.crypto
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.sync.{minFee, setScriptFee, transferAmount}
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.Proofs
import com.zbsnetwork.transaction.lease.LeaseTransactionV2
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.smart.script.ScriptCompiler
import org.scalatest.CancelAfterFailure

class BigString extends BaseTransactionSuite with CancelAfterFailure {
  private val acc0 = pkByAddress(firstAddress)
  private val acc1 = pkByAddress(secondAddress)
  private val acc2 = pkByAddress(thirdAddress)

  test("set contract, make leasing and cancel leasing") {
    val (balance1, eff1) = notMiner.accountBalances(acc0.address)
    val (balance2, eff2) = notMiner.accountBalances(thirdAddress)

    val txId = sender.transfer(sender.address, acc0.address, 10 * transferAmount, minFee).id
    nodes.waitForHeightAriseAndTxPresent(txId)

    notMiner.assertBalances(firstAddress, balance1 + 10 * transferAmount, eff1 + 10 * transferAmount)

    val scriptText = s"""
        let pkA = base58'${ByteStr(acc0.publicKey)}'
        let pkB = base58'${ByteStr(acc1.publicKey)}'
        let pkC = base58'${ByteStr(acc2.publicKey)}'

        let a0 = "йцукенгшщзхъфывапролдячсмитьбюйцукпврарвараравртавтрвапваппвпавп"
        ${(for (b <- 1 to 20) yield { "let a" + b + "=a" + (b - 1) + "+a" + (b - 1) }).mkString("\n")}

        a20 == a0 || match tx {
          case ltx: LeaseTransaction => sigVerify(ltx.bodyBytes,ltx.proofs[0],pkA) && sigVerify(ltx.bodyBytes,ltx.proofs[2],pkC)
          case lctx : LeaseCancelTransaction => sigVerify(lctx.bodyBytes,lctx.proofs[1],pkA) && sigVerify(lctx.bodyBytes,lctx.proofs[2],pkB)
          case other => false
        }
        """.stripMargin

    val script = ScriptCompiler(scriptText, isAssetScript = false).explicitGet()._1
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(acc0, Some(script), setScriptFee, System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json())
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val unsignedLeasing =
      LeaseTransactionV2
        .create(
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

    assertBadRequestAndMessage(sender.signedBroadcast(signedLeasing.json()).id, "String is too large")

    val leasingId = Base58.encode(unsignedLeasing.id().arr)

    nodes.waitForHeightArise()
    nodes(0).findTransactionInfo(leasingId) shouldBe None

    notMiner.assertBalances(firstAddress, balance1 + 10 * transferAmount - setScriptFee, eff1 + 10 * transferAmount - setScriptFee)
    notMiner.assertBalances(thirdAddress, balance2, eff2)

  }
}
