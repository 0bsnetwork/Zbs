package com.zbsplatform.transaction.lease

import com.google.common.primitives.Bytes
import com.zbsplatform.crypto
import com.zbsplatform.state._
import monix.eval.Coeval
import com.zbsplatform.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.zbsplatform.transaction.ValidationError.{GenericError, UnsupportedVersion}
import com.zbsplatform.transaction._

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransactionV2 private (version: Byte,
                                             chainId: Byte,
                                             sender: PublicKeyAccount,
                                             leaseId: ByteStr,
                                             fee: Long,
                                             timestamp: Long,
                                             proofs: Proofs)
    extends LeaseCancelTransaction
    with FastHashId {

  override def chainByte: Option[Byte] = Some(chainId)

  override val builder: TransactionParser = LeaseCancelTransactionV2

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Bytes.concat(Array(builder.typeId, version, chainId), bytesBase()))

  override val bytes = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

}

object LeaseCancelTransactionV2 extends TransactionParserFor[LeaseCancelTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte                 = 9
  override def supportedVersions: Set[Byte] = Set(2)
  private def networkByte                   = AddressScheme.current.chainId

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val chainId                                = bytes(0)
      val (sender, fee, timestamp, leaseId, end) = LeaseCancelTransaction.parseBase(bytes, 1)
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- LeaseCancelTransactionV2.create(version, chainId, sender, leaseId, fee, timestamp, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] =
    for {
      _ <- Either.cond(supportedVersions.contains(version), (), UnsupportedVersion(version))
      _ <- Either.cond(chainId == networkByte, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $networkByte"))
      _ <- LeaseCancelTransaction.validateLeaseCancelParams(leaseId, fee)
    } yield LeaseCancelTransactionV2(version, chainId, sender, leaseId, fee, timestamp, proofs)

  def signed(version: Byte,
             chainId: Byte,
             sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(version, chainId, sender, leaseId, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(version: Byte,
                 chainId: Byte,
                 sender: PrivateKeyAccount,
                 leaseId: ByteStr,
                 fee: Long,
                 timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(version, chainId, sender, leaseId, fee, timestamp, sender)
  }
}
