package com.zbsnetwork.api.http.assets

import cats.implicits._
import com.zbsnetwork.account.{AddressScheme, PublicKeyAccount}
import com.zbsnetwork.api.http.BroadcastRequest
import com.zbsnetwork.transaction.assets.ReissueTransactionV2
import com.zbsnetwork.transaction.{AssetIdStringLength, Proofs, ValidationError}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

case class SignedReissueV2Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                  senderPublicKey: String,
                                  @ApiModelProperty(required = true)
                                  version: Byte,
                                  @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                  assetId: String,
                                  @ApiModelProperty(required = true, example = "1000000")
                                  quantity: Long,
                                  @ApiModelProperty(required = true)
                                  reissuable: Boolean,
                                  @ApiModelProperty(required = true)
                                  fee: Long,
                                  @ApiModelProperty(required = true)
                                  timestamp: Long,
                                  @ApiModelProperty(required = true)
                                  proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ReissueTransactionV2] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      chainId = AddressScheme.current.chainId
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _assetId    <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _t          <- ReissueTransactionV2.create(version, chainId, _sender, _assetId, quantity, reissuable, fee, timestamp, _proofs)
    } yield _t
}

object SignedReissueV2Request {
  implicit val assetReissueRequestReads: Reads[SignedReissueV2Request] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "version").read[Byte] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "proofs").read[List[ProofStr]]
  )(SignedReissueV2Request.apply _)
}
