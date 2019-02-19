package com.zbsnetwork.transaction.lease

import com.google.common.primitives.Bytes
import com.zbsnetwork.account.{AddressScheme, PrivateKeyAccount, PublicKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.crypto
import com.zbsnetwork.transaction.ValidationError.GenericError
import com.zbsnetwork.transaction._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

case class LeaseCancelTransactionV2 private (chainId: Byte, sender: PublicKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long, proofs: Proofs)
    extends LeaseCancelTransaction
    with FastHashId {

  override def chainByte: Option[Byte] = Some(chainId)

  override val builder: TransactionParser = LeaseCancelTransactionV2

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(Bytes.concat(Array(builder.typeId, version, chainId), bytesBase()))

  override val bytes = Coeval.evalOnce(Bytes.concat(Array(0: Byte), bodyBytes(), proofs.bytes()))

  override def version: Byte = 2
}

object LeaseCancelTransactionV2 extends TransactionParserFor[LeaseCancelTransactionV2] with TransactionParser.MultipleVersions {

  override val typeId: Byte = LeaseCancelTransaction.typeId

  override def supportedVersions: Set[Byte] = Set(2)
  private def currentChainId: Byte          = AddressScheme.current.chainId

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    Try {
      val chainId                                = bytes(0)
      val (sender, fee, timestamp, leaseId, end) = LeaseCancelTransaction.parseBase(bytes, 1)
      (for {
        proofs <- Proofs.fromBytes(bytes.drop(end))
        tx     <- LeaseCancelTransactionV2.create(chainId, sender, leaseId, fee, timestamp, proofs)
      } yield tx).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten
  }

  def create(chainId: Byte,
             sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    for {
      _ <- Either.cond(chainId == currentChainId, (), GenericError(s"Wrong chainId actual: ${chainId.toInt}, expected: $currentChainId"))
      _ <- LeaseCancelTransaction.validateLeaseCancelParams(leaseId, fee)
    } yield LeaseCancelTransactionV2(chainId, sender, leaseId, fee, timestamp, proofs)
  }

  def signed(chainId: Byte,
             sender: PublicKeyAccount,
             leaseId: ByteStr,
             fee: Long,
             timestamp: Long,
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(chainId, sender, leaseId, fee, timestamp, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(chainId: Byte, sender: PrivateKeyAccount, leaseId: ByteStr, fee: Long, timestamp: Long): Either[ValidationError, TransactionT] = {
    signed(chainId, sender, leaseId, fee, timestamp, sender)
  }
}
