package com.zbsplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.zbsplatform.state.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.zbsplatform.account.PublicKeyAccount
import com.zbsplatform.transaction.validation._
import com.zbsplatform.transaction.{AssetId, ProvenTransaction, ValidationError, _}
import scorex.crypto.signatures.Curve25519._

trait ReissueTransaction extends ProvenTransaction with VersionedTransaction {
  def assetId: ByteStr
  def quantity: Long
  def reissuable: Boolean
  def fee: Long
  def chainByte: Option[Byte]

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "chainId"    -> chainByte,
      "assetId"    -> assetId.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
}

object ReissueTransaction {
  def validateReissueParams(quantity: Long, fee: Long): Either[ValidationError, Unit] =
    (validateAmount(quantity, "assets"), validateFee(fee))
      .mapN { case _ => () }
      .leftMap(_.head)
      .toEither

  def parseBase(bytes: Array[Byte], start: Int): (PublicKeyAccount, AssetId, Long, Boolean, Long, Long, Int) = {
    val senderEnd  = start + KeyLength
    val assetIdEnd = senderEnd + AssetIdLength
    val sender     = PublicKeyAccount(bytes.slice(start, senderEnd))
    val assetId    = ByteStr(bytes.slice(senderEnd, assetIdEnd))
    val quantity   = Longs.fromByteArray(bytes.slice(assetIdEnd, assetIdEnd + 8))
    val reissuable = bytes.slice(assetIdEnd + 8, assetIdEnd + 9).head == (1: Byte)
    val fee        = Longs.fromByteArray(bytes.slice(assetIdEnd + 9, assetIdEnd + 17))
    val end        = assetIdEnd + 25
    val timestamp  = Longs.fromByteArray(bytes.slice(assetIdEnd + 17, end))

    (sender, assetId, quantity, reissuable, fee, timestamp, end)
  }
}
