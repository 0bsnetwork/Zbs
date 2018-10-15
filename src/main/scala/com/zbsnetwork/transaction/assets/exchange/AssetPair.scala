package com.zbsplatform.transaction.assets.exchange

import com.zbsplatform.state.ByteStr
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsObject, Json}
import com.zbsplatform.transaction._
import com.zbsplatform.transaction.assets.exchange.Order.assetIdBytes
import com.zbsplatform.transaction.assets.exchange.Validation.booleanOperators

import scala.util.{Success, Try}

case class AssetPair(@ApiModelProperty(dataType = "java.lang.String") amountAsset: Option[AssetId],
                     @ApiModelProperty(dataType = "java.lang.String") priceAsset: Option[AssetId]) {
  import AssetPair._
  @ApiModelProperty(hidden = true)
  lazy val priceAssetStr: String = assetIdStr(priceAsset)
  @ApiModelProperty(hidden = true)
  lazy val amountAssetStr: String = assetIdStr(amountAsset)

  override def toString: String = key

  def key: String = amountAssetStr + "-" + priceAssetStr

  def isValid: Validation = (amountAsset != priceAsset) :| "Invalid AssetPair"

  def bytes: Array[Byte] = assetIdBytes(amountAsset) ++ assetIdBytes(priceAsset)

  def json: JsObject = Json.obj(
    "amountAsset" -> amountAsset.map(_.base58),
    "priceAsset"  -> priceAsset.map(_.base58)
  )

  def reverse = AssetPair(priceAsset, amountAsset)

  def assets: Set[Option[AssetId]] = Set(amountAsset, priceAsset)
}

object AssetPair {
  val ZbsName = "ZBS"

  def assetIdStr(aid: Option[AssetId]): String = aid.fold(ZbsName)(_.base58)

  def extractAssetId(a: String): Try[Option[AssetId]] = a match {
    case `ZbsName` => Success(None)
    case other     => ByteStr.decodeBase58(other).map(Option(_))
  }

  def createAssetPair(amountAsset: String, priceAsset: String): Try[AssetPair] =
    for {
      a1 <- extractAssetId(amountAsset)
      a2 <- extractAssetId(priceAsset)
    } yield AssetPair(a1, a2)
}
