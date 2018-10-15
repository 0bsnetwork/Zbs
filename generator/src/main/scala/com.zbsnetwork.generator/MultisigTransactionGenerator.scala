package com.zbsplatform.generator
import cats.Show
import com.zbsplatform.crypto
import com.zbsplatform.generator.utils.Gen
import com.zbsplatform.state._
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.Script
import com.zbsplatform.transaction.transfer.TransferTransactionV2
import com.zbsplatform.transaction.{Proofs, Transaction}
import com.zbsplatform.it.util._
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

    val setScript = SetScriptTransaction.selfSigned(1, bank, Some(script), enoughFee, System.currentTimeMillis()).explicitGet()

    val res = Range(0, settings.transactions).map { i =>
      val tx = TransferTransactionV2
        .create(2,
                None,
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
