package com.zbsnetwork.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import com.zbsnetwork.account.{Alias, PublicKeyAccount}
import com.zbsnetwork.api.http.BroadcastRequest
import com.zbsnetwork.transaction.{CreateAliasTransaction, CreateAliasTransactionV2, Proofs, ValidationError}
import cats.implicits._

case class SignedCreateAliasV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                      senderPublicKey: String,
                                      @ApiModelProperty(required = true)
                                      fee: Long,
                                      @ApiModelProperty(value = "Alias", required = true)
                                      alias: String,
                                      @ApiModelProperty(required = true)
                                      timestamp: Long,
                                      @ApiModelProperty(required = true)
                                      proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _alias      <- Alias.buildWithCurrentChainId(alias)
      _t          <- CreateAliasTransactionV2.create(_sender, _alias, fee, timestamp, _proofs)
    } yield _t
}

object SignedCreateAliasV2Request {
  implicit val broadcastAliasV2RequestReadsFormat: Format[SignedCreateAliasV2Request] = Json.format
}
