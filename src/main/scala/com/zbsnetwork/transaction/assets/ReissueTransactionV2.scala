package com.zbsplatform.transaction.assets

import com.google.common.primitives.Bytes
import com.zbsplatform.crypto
import com.zbsplatform.state.ByteStr
import monix.eval.Coeval
import com.zbsplatform.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.zbsplatform.transaction.ValidationError.{GenericError, UnsupportedVersion}
import com.zbsplatform.transaction._

import scala.util._

case class ReissueTransactionV2 private (version: Byte,
                                         chainId: Byte,
                                         sender: PublicKeyAccount,
                                         assetId: ByteStr,
                                         quantity: Long,
                                         reissuable: Boolean,
                                         fee: Long,
                                         timestamp: Long,
                                         proofs: Proofs)
    extends ReissueTransaction
    with FastHashId
    with ChainSpecific {
  override val builder: TransactionParser = ReissueTransactionV2
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce(
    Bytes.concat(
      Array(builder.typeId, version, chainId),
      bytesBase(),
    ))
  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def chainByte: Option[Byte] = Some(chainId)
}

object ReissueTransactionV2 extends TransactionParserFor[ReissueTransactionV2] with TransactionParser.MultipleVersions {
  override val typeId: Byte                 = 5
  override def supportedVersions: Set[Byte] = Set(2)
  private def networkByte                   = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId                                                      = bytes(0)
      val (sender, assetId, quantity, reissuable, fee, timestamp, end) = ReissueTransaction.parseBase(bytes, 1)
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx <- ReissueTransactionV2
          .create(version, chainId, sender, assetId, quantity, reissuable, fee, timestamp, proofs)
      } yield tx)
        .fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), UnsupportedVersion(version))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $networkByte"))
      _ <- ReissueTransaction.validateReissueParams(quantity, fee)
    } yield ReissueTransactionV2(version, chainId, sender, assetId, quantity, reissuable, fee, timestamp, proofs)

  def signed(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             assetId: ByteStr,
             quantity: Long,
             reissuable: Boolean,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] =
    for {
      unverified <- create(version, chainId, sender, assetId, quantity, reissuable, fee, timestamp, Proofs.empty)
      proofs     <- Proofs.create(Seq(ByteStr(crypto.sign(signer, unverified.bodyBytes()))))
    } yield unverified.copy(proofs = proofs)

  def selfSigned(version: Byte,
                 chainId: Byte,
                 sender: PrivateKeyAccount,
                 assetId: ByteStr,
                 quantity: Long,
                 reissuable: Boolean,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] =
    signed(version, chainId, sender, assetId, quantity, reissuable, fee, timestamp, sender)
}
