package com.zbsnetwork.api.http

import cats.implicits._
import com.zbsnetwork.state.DataEntry
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import scala.annotation.meta.field
import play.api.libs.json.Json
import com.zbsnetwork.account.PublicKeyAccount
import com.zbsnetwork.transaction.{DataTransaction, Proofs, ValidationError}

object DataRequest {
  implicit val unsignedDataRequestReads = Json.reads[DataRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedDataRequest]
}

case class DataRequest(@(ApiModelProperty @field)(required = true, dataType = "java.lang.Integer", value = "1", allowableValues = "1") version: Byte,
                       sender: String,
                       @(ApiModelProperty @field)(required = true) data: List[DataEntry[_]],
                       @(ApiModelProperty @field)(required = true, value = "1000") fee: Long,
                       timestamp: Option[Long] = None)

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@(ApiModelProperty @field)(required = true, dataType = "java.lang.Integer", value = "1", allowableValues = "1")
                             version: Byte,
                             @(ApiModelProperty @field)(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @(ApiModelProperty @field)(value = "Data to put into blockchain", required = true)
                             data: List[DataEntry[_]],
                             @(ApiModelProperty @field)(required = true)
                             fee: Long,
                             @(ApiModelProperty @field)(required = true, value = "1000")
                             timestamp: Long,
                             @(ApiModelProperty @field)(required = true)
                             proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- DataTransaction.create(version, _sender, data, fee, timestamp, _proofs)
    } yield t
}
