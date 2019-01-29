package com.zbsnetwork.api.http.leasing

import cats.implicits._
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._
import play.api.libs.functional.syntax._
import com.zbsnetwork.account.PublicKeyAccount
import com.zbsnetwork.api.http.BroadcastRequest
import com.zbsnetwork.transaction.TransactionParsers.SignatureStringLength
import com.zbsnetwork.transaction.lease.LeaseCancelTransactionV2
import com.zbsnetwork.transaction.{Proofs, ValidationError}

case class SignedLeaseCancelV2Request(@ApiModelProperty(required = true)
                                      version: Byte,
                                      @ApiModelProperty(required = true)
                                      chainId: Byte,
                                      @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                      leaseId: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      proofs: List[String],
                                      @ApiModelProperty(required = true)
                                      fee: Long)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseCancelTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _leaseTx    <- parseBase58(leaseId, "invalid.leaseTx", SignatureStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _t          <- LeaseCancelTransactionV2.create(version, chainId, _sender, _leaseTx, fee, timestamp, _proofs)
    } yield _t
}
object SignedLeaseCancelV2Request {
  implicit val reads: Reads[SignedLeaseCancelV2Request] = (
    (JsPath \ "version").read[Byte] and
      (JsPath \ "chainId").read[Byte] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "leaseId").read[String] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[String]] and
      (JsPath \ "fee").read[Long]
  )(SignedLeaseCancelV2Request.apply _)

  implicit val writes = Json.writes[SignedLeaseCancelV2Request]
}
