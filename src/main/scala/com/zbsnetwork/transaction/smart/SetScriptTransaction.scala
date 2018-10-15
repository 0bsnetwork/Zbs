package com.zbsplatform.transaction.smart

import com.google.common.primitives.{Bytes, Longs}
import com.zbsplatform.account._
import com.zbsplatform.crypto
import com.zbsplatform.serialization.Deser
import com.zbsplatform.state._
import com.zbsplatform.transaction.ValidationError.GenericError
import com.zbsplatform.transaction._
import com.zbsplatform.transaction.smart.script.{Script, ScriptReader}
import monix.eval.Coeval
import play.api.libs.json.Json
import scorex.crypto.signatures.Curve25519.KeyLength

import scala.util.{Failure, Success, Try}

case class SetScriptTransaction private (version: Byte,
                                         chainId: Byte,
                                         sender: PublicKeyAccount,
                                         script: Option[Script],
                                         fee: Long,
                                         timestamp: Long,
                                         proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: TransactionParser = SetScriptTransaction

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      sender.publicKey,
      Deser.serializeOption(script)(s => s.bytes().arr),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    ))

  override val assetFee = (None, fee)
  override val json     = Coeval.evalOnce(jsonBase() ++ Json.obj("version" -> version, "script" -> script.map(_.bytes().base64)))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))
}

object SetScriptTransaction extends TransactionParserFor[SetScriptTransaction] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 13
  override val supportedVersions: Set[Byte] = Set(1)

  private def networkByte = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId = bytes(0)
      val sender  = PublicKeyAccount(bytes.slice(1, KeyLength + 1))
      val (scriptOptEi: Option[Either[ValidationError.ScriptParseError, Script]], scriptEnd) =
        Deser.parseOption(bytes, KeyLength + 1)(ScriptReader.fromBytes)
      val scriptEiOpt = scriptOptEi match {
        case None            => Right(None)
        case Some(Right(sc)) => Right(Some(sc))
        case Some(Left(err)) => Left(err)
      }

      val fee       = Longs.fromByteArray(bytes.slice(scriptEnd, scriptEnd + 8))
      val timestamp = Longs.fromByteArray(bytes.slice(scriptEnd + 8, scriptEnd + 16))
      (for {
        scriptOpt <- scriptEiOpt
        _         <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId ${chainId.toInt}"))
        proofs    <- Proofs.fromBytes(bytes.drop(scriptEnd + 16))
        tx        <- create(version, sender, scriptOpt, fee, timestamp, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             sender: PublicKeyAccount,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), ValidationError.UnsupportedVersion(version))
      _ <- Either.cond(fee > 0, (), ValidationError.InsufficientFee(s"insufficient fee: $fee"))
    } yield new SetScriptTransaction(version, networkByte, sender, script, fee, timestamp, proofs)

  def signed(version: Byte,
             sender: PublicKeyAccount,
             script: Option[Script],
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    create(version, sender, script, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }

  def selfSigned(version: Byte,
                 sender: PrivateKeyAccount,
                 script: Option[Script],
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, sender, script, fee, timestamp, sender)
}
