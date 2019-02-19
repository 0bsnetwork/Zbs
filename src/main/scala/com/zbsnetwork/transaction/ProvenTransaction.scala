package com.zbsnetwork.transaction

import com.zbsnetwork.common.utils.Base58
import play.api.libs.json._

trait ProvenTransaction extends Transaction with Proven {

  protected def proofField: Seq[(String, JsValue)] = Seq("proofs" -> JsArray(this.proofs.proofs.map(p => JsString(p.base58))))

  protected def jsonBase(): JsObject =
    Json.obj(
      "type"            -> builder.typeId,
      "id"              -> id().base58,
      "sender"          -> sender.address,
      "senderPublicKey" -> Base58.encode(sender.publicKey),
      "fee"             -> assetFee._2,
      "timestamp"       -> timestamp
    ) ++ JsObject(proofField)
}
