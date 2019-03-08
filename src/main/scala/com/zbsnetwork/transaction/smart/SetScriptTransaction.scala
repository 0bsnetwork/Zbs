package com.zbsnetwork.transaction.smart

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.zbsnetwork.account._
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.crypto
import com.zbsnetwork.serialization.Deser
import com.zbsnetwork.transaction.ValidationError.GenericError
import com.zbsnetwork.transaction._
import com.zbsnetwork.transaction.description._
import com.zbsnetwork.transaction.smart.script.Script
import monix.eval.Coeval
import play.api.libs.json.Json

import scala.util.Try

case class SetScriptTransaction private (chainId: Byte, sender: PublicKeyAccount, script: Option[Script], fee: Long, timestamp: Long, proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = SetScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Bytes.concat(
        Array(builder.typeId, version, chainId),
        sender.publicKey,
        Deser.serializeOption(script)(s => s.bytes().arr),
        Longs.toByteArray(fee),
        Longs.toByteArray(timestamp)
      )
    )

  override val assetFee: (Option[AssetId], Long) = (None, fee)
  override val json                              = Coeval.evalOnce(jsonBase() ++ Json.obj("chainId" -> chainId, "version" -> version, "script" -> script.map(_.bytes().base64)))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
  override def version: Byte              = 1
}

object SetScriptTransaction extends TransactionParserFor[SetScriptTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 13
  override val supportedVersions: Set[Byte] = Set(1)

  private def chainId: Byte = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Either
        .cond(tx.chainId == chainId, (), GenericError(s"Wrong chainId ${tx.chainId.toInt}"))
        .flatMap(_ => Either.cond(tx.fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: ${tx.fee}")))
        .map(_ => tx)
        .foldToTry
    }
  }

  def create(sender: PublicKeyAccount, script: Option[Script], fee: Long, timestamp: Long, proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
    } yield new SetScriptTransaction(chainId, sender, script, fee, timestamp, proofs)
  }

  def signed(sender: PublicKeyAccount,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(sender, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(sender: PrivateKeyAccount, script: Option[Script], fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(sender, script, fee, timestamp, sender)
  }

  val byteTailDescription: ByteEntity[SetScriptTransaction] = {
    (
      OneByte(tailIndex(1), "Chain ID"),
      PublicKeyAccountBytes(tailIndex(2), "Sender's public key"),
      OptionBytes(index = tailIndex(3), name = "Script", nestedByteEntity = ScriptBytes(tailIndex(3), "Script")),
      LongBytes(tailIndex(4), "Fee"),
      LongBytes(tailIndex(5), "Timestamp"),
      ProofsBytes(tailIndex(6))
    ) mapN SetScriptTransaction.apply
  }
}
