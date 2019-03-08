package com.zbsnetwork.generator

import cats.Show
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.generator.OracleTransactionGenerator.Settings
import com.zbsnetwork.generator.utils.Gen
import com.zbsnetwork.it.util._
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.transfer.TransferTransactionV2
import com.zbsnetwork.transaction.{DataTransaction, Transaction}

class OracleTransactionGenerator(settings: Settings, val accounts: Seq[PrivateKeyAccount]) extends TransactionGenerator {
  override def next(): Iterator[Transaction] = generate(settings).toIterator

  def generate(settings: Settings): Seq[Transaction] = {
    val oracle = accounts.last

    val scriptedAccount = accounts.head

    val script = Gen.oracleScript(oracle, settings.requiredData)

    val enoughFee = 0.005.zbs

    val setScript: Transaction =
      SetScriptTransaction
        .selfSigned(scriptedAccount, Some(script), enoughFee, System.currentTimeMillis())
        .explicitGet()

    val setDataTx: Transaction = DataTransaction
      .selfSigned(oracle, settings.requiredData.toList, enoughFee, System.currentTimeMillis())
      .explicitGet()

    val now = System.currentTimeMillis()
    val transactions: List[Transaction] = (1 to settings.transactions).map { i =>
      TransferTransactionV2
        .selfSigned(None, scriptedAccount, oracle, 1.zbs, now + i, None, enoughFee, Array.emptyByteArray)
        .explicitGet()
    }.toList

    setScript +: setDataTx +: transactions
  }
}

object OracleTransactionGenerator {
  final case class Settings(transactions: Int, requiredData: Set[DataEntry[_]])

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      s"Transactions: ${x.transactions}\n" +
        s"DataEntries: ${x.requiredData}\n"
    }
  }
}
