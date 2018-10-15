package com.zbsplatform.transaction.smart

import cats.syntax.all._
import com.zbsplatform.crypto
import com.zbsplatform.state._
import com.zbsplatform.transaction.ValidationError.{GenericError, ScriptExecutionError, TransactionNotAllowedByScript}
import com.zbsplatform.transaction._
import com.zbsplatform.transaction.assets._
import com.zbsplatform.transaction.smart.script.{Script, ScriptRunner}
import com.zbsplatform.transaction.transfer._

object Verifier {

  def apply(blockchain: Blockchain, currentBlockHeight: Int)(tx: Transaction): Either[ValidationError, Transaction] =
    (tx match {
      case _: GenesisTransaction => Right(tx)
      case pt: ProvenTransaction =>
        (pt, blockchain.accountScript(pt.sender)) match {
          case (stx: SignedTransaction, None)       => stx.signaturesValid()
          case (_: SignedTransaction, Some(_))      => Left(GenericError("Can't process transaction  with signature from scripted account"))
          case (_: ProvenTransaction, Some(script)) => verify(blockchain, script, currentBlockHeight, pt, false)
          case (_: ProvenTransaction, None)         => verifyAsEllipticCurveSignature(pt)
        }
    }).flatMap(tx => {
      for {
        assetId <- tx match {
          case t: TransferTransaction     => t.assetId
          case t: MassTransferTransaction => t.assetId
          case t: BurnTransaction         => Some(t.assetId)
          case t: ReissueTransaction      => Some(t.assetId)
          case _                          => None
        }

        script <- blockchain.assetDescription(assetId).flatMap(_.script)
      } yield verify(blockchain, script, currentBlockHeight, tx, true)
    }.getOrElse(Either.right(tx)))

  def verify[T <: Transaction](blockchain: Blockchain,
                               script: Script,
                               height: Int,
                               transaction: T,
                               isTokenScript: Boolean): Either[ValidationError, T] = {
    ScriptRunner[Boolean, T](height, transaction, blockchain, script) match {
      case (log, Left(execError)) => Left(ScriptExecutionError(execError, script.text, log, isTokenScript))
      case (log, Right(false))    => Left(TransactionNotAllowedByScript(log, script.text, isTokenScript))
      case (_, Right(true))       => Right(transaction)
    }
  }

  def verifyAsEllipticCurveSignature[T <: ProvenTransaction](pt: T): Either[ValidationError, T] =
    pt.proofs.proofs match {
      case p :: Nil =>
        Either.cond(crypto.verify(p.arr, pt.bodyBytes(), pt.sender.publicKey),
                    pt,
                    GenericError(s"Script doesn't exist and proof doesn't validate as signature for $pt"))
      case _ => Left(GenericError("Transactions from non-scripted accounts must have exactly 1 proof"))
    }

}
