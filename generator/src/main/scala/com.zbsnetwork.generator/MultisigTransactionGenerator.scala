package com.zbsnetwork.generator
import cats.Show
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.crypto
import com.zbsnetwork.generator.utils.Gen
import com.zbsnetwork.it.util._
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.smart.script.Script
import com.zbsnetwork.transaction.transfer.TransferTransactionV2
import com.zbsnetwork.transaction.{Proofs, Transaction}

import scala.util.Random

class MultisigTransactionGenerator(settings: MultisigTransactionGenerator.Settings, val accounts: Seq[PrivateKeyAccount])
    extends TransactionGenerator {

  override def next(): Iterator[Transaction] = {
    generate(settings).toIterator
  }

  private def generate(settings: MultisigTransactionGenerator.Settings): Seq[Transaction] = {

    val bank   = accounts.head
    val owners = Seq(createAccount(), accounts(1), createAccount(), accounts(2), createAccount(), accounts(3), createAccount(), createAccount())

    val enoughFee               = 0.005.zbs
    val totalAmountOnNewAccount = 1.zbs

    val script: Script = Gen.multiSigScript(owners, 3)

    val setScript = SetScriptTransaction.selfSigned(bank, Some(script), enoughFee, System.currentTimeMillis()).explicitGet()

    val res = Range(0, settings.transactions).map { i =>
      val tx = TransferTransactionV2
        .create(None,
                bank,
                owners(1),
                totalAmountOnNewAccount - 2 * enoughFee - i,
                System.currentTimeMillis(),
                None,
                enoughFee,
                Array.emptyByteArray,
                Proofs.empty)
        .explicitGet()
      val signatures = owners.map(crypto.sign(_, tx.bodyBytes())).map(ByteStr(_))
      tx.copy(proofs = Proofs(signatures))
    }

    println(System.currentTimeMillis())
    println(s"${res.length} tx generated")

    if (settings.firstRun) setScript +: res
    else res
  }

  private def createAccount() = {
    val seedBytes = Array.fill(32)(0: Byte)
    Random.nextBytes(seedBytes)
    PrivateKeyAccount(seedBytes)
  }
}

object MultisigTransactionGenerator {
  final case class Settings(transactions: Int, firstRun: Boolean)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"""
        | transactions = ${x.transactions}
        | firstRun = ${x.firstRun}
      """.stripMargin
    }
  }
}
