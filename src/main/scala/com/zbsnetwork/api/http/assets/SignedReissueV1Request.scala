package com.zbsplatform.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.zbsplatform.account.PublicKeyAccount
import com.zbsplatform.api.http.BroadcastRequest
import com.zbsplatform.transaction.TransactionParsers.SignatureStringLength
import com.zbsplatform.transaction.assets.ReissueTransactionV1
import com.zbsplatform.transaction.{AssetIdStringLength, ValidationError}

object SignedReissueV1Request {
  implicit val assetReissueRequestReads: Format[SignedReissueV1Request] = Json.format
}

case class SignedReissueV1Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                  senderPublicKey: String,
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
                                  signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, ReissueTransactionV1] =
    for {
      _sender    <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _assetId   <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _t         <- ReissueTransactionV1.create(_sender, _assetId, quantity, reissuable, fee, timestamp, _signature)
    } yield _t
}
