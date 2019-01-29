package com.zbsnetwork.api.http.assets

import cats.implicits._
import com.zbsnetwork.account.{AddressScheme, PublicKeyAccount}
import com.zbsnetwork.api.http.BroadcastRequest
import com.zbsnetwork.transaction.assets.BurnTransactionV2
import com.zbsnetwork.transaction.{AssetIdStringLength, Proofs, ValidationError}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Json, Reads, Writes}

case class SignedBurnV2Request(@ApiModelProperty(value = "BurnTransaction format version", required = true)
                               version: Byte,
                               @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                               senderPublicKey: String,
                               @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                               assetId: String,
                               @ApiModelProperty(required = true, example = "1000000")
                               quantity: Long,
                               @ApiModelProperty(required = true)
                               fee: Long,
                               @ApiModelProperty(required = true)
                               timestamp: Long,
                               @ApiModelProperty(required = true)
                               proofs: List[String])
    extends BroadcastRequest {

  def toTx: Either[ValidationError, BurnTransactionV2] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      chainId = AddressScheme.current.chainId
      _t <- BurnTransactionV2.create(version, chainId, _sender, _assetId, quantity, fee, timestamp, _proofs)
    } yield _t
}

object SignedBurnV2Request {
  implicit val reads: Reads[SignedBurnV2Request] = (
    (JsPath \ "version").read[Byte] and
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long].orElse((JsPath \ "amount").read[Long]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedBurnV2Request.apply _)

  implicit val writes: Writes[SignedBurnV2Request] = Json.writes[SignedBurnV2Request]
}
