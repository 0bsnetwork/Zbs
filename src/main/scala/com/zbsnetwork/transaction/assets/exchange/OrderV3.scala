package com.zbsnetwork.transaction.assets.exchange

import cats.data.State
import com.google.common.primitives.Longs
import com.zbsnetwork.account.{PrivateKeyAccount, PublicKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.Base58
import com.zbsnetwork.crypto
import com.zbsnetwork.crypto.KeyLength
import com.zbsnetwork.serialization.Deser
import com.zbsnetwork.transaction.{AssetId, Proofs}
import com.zbsnetwork.utils.byteStrWrites
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

import scala.util.Try

case class OrderV3(senderPublicKey: PublicKeyAccount,
                   matcherPublicKey: PublicKeyAccount,
                   assetPair: AssetPair,
                   orderType: OrderType,
                   amount: Long,
                   price: Long,
                   timestamp: Long,
                   expiration: Long,
                   matcherFee: Long,
                   override val matcherFeeAssetId: Option[AssetId],
                   proofs: Proofs)
    extends Order {

  def version: Byte = 3

  override def signature: Array[Byte] = proofs.proofs.head.arr

  val bodyBytes: Coeval[Array[Byte]] =
    Coeval.evalOnce(
      Array(version) ++
        senderPublicKey.publicKey ++
        matcherPublicKey.publicKey ++
        assetPair.bytes ++
        orderType.bytes ++
        Longs.toByteArray(amount) ++
        Longs.toByteArray(price) ++
        Longs.toByteArray(timestamp) ++
        Longs.toByteArray(expiration) ++
        Longs.toByteArray(matcherFee) ++
        Order.assetIdBytes(matcherFeeAssetId)
    )

  val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(bodyBytes() ++ proofs.bytes())

  override val json: Coeval[JsObject] =
    Coeval.evalOnce(
      {
        val sig = Base58.encode(signature)
        Json.obj(
          "version"           -> version,
          "id"                -> idStr(),
          "sender"            -> senderPublicKey.address,
          "senderPublicKey"   -> Base58.encode(senderPublicKey.publicKey),
          "matcherPublicKey"  -> Base58.encode(matcherPublicKey.publicKey),
          "assetPair"         -> assetPair.json,
          "orderType"         -> orderType.toString,
          "amount"            -> amount,
          "price"             -> price,
          "timestamp"         -> timestamp,
          "expiration"        -> expiration,
          "matcherFee"        -> matcherFee,
          "matcherFeeAssetId" -> matcherFeeAssetId.map(_.base58),
          "signature"         -> sig,
          "proofs"            -> proofs.proofs
        )
      }
    )
}

object OrderV3 {

  private val AssetIdLength = 32

  def buy(sender: PrivateKeyAccount,
          matcher: PublicKeyAccount,
          pair: AssetPair,
          amount: Long,
          price: Long,
          timestamp: Long,
          expiration: Long,
          matcherFee: Long,
          matcherFeeAssetId: Option[AssetId]): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.BUY, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def sell(sender: PrivateKeyAccount,
           matcher: PublicKeyAccount,
           pair: AssetPair,
           amount: Long,
           price: Long,
           timestamp: Long,
           expiration: Long,
           matcherFee: Long,
           matcherFeeAssetId: Option[AssetId]): Order = {

    val unsigned = OrderV3(sender, matcher, pair, OrderType.SELL, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def apply(sender: PrivateKeyAccount,
            matcher: PublicKeyAccount,
            pair: AssetPair,
            orderType: OrderType,
            amount: Long,
            price: Long,
            timestamp: Long,
            expiration: Long,
            matcherFee: Long,
            matcherFeeAssetId: Option[AssetId]): Order = {

    val unsigned = OrderV3(sender, matcher, pair, orderType, amount, price, timestamp, expiration, matcherFee, matcherFeeAssetId, Proofs.empty)
    val sig      = crypto.sign(sender, unsigned.bodyBytes())

    unsigned.copy(proofs = Proofs(Seq(ByteStr(sig))))
  }

  def parseBytes(bytes: Array[Byte]): Try[Order] = Try {

    val longLength = 8

    val readByte: State[Int, Byte] = State { from =>
      (from + 1, bytes(from))
    }

    def read[T](f: Array[Byte] => T, size: Int): State[Int, T] = State { from =>
      val end = from + size
      (end, f(bytes.slice(from, end)))
    }

    def readEnd[T](f: Array[Byte] => T): State[Int, T] = State { from =>
      (from, f(bytes.drop(from)))
    }

    def parse[T](f: (Array[Byte], Int, Int) => (T, Int), size: Int): State[Int, T] = State { from =>
      val (res, off) = f(bytes, from, size)
      (off, res)
    }

    val makeOrder = for {
      version <- readByte
      _ = if (version != 3) { throw new Exception(s"Incorrect order version: expect 3 but found $version") }
      sender            <- read(PublicKeyAccount.apply, KeyLength)
      matcher           <- read(PublicKeyAccount.apply, KeyLength)
      amountAssetId     <- parse(Deser.parseByteArrayOption, AssetIdLength)
      priceAssetId      <- parse(Deser.parseByteArrayOption, AssetIdLength)
      orderType         <- readByte
      amount            <- read(Longs.fromByteArray, longLength)
      price             <- read(Longs.fromByteArray, longLength)
      timestamp         <- read(Longs.fromByteArray, longLength)
      expiration        <- read(Longs.fromByteArray, longLength)
      matcherFee        <- read(Longs.fromByteArray, longLength)
      matcherFeeAssetId <- parse(Deser.parseByteArrayOption, AssetIdLength)
      maybeProofs       <- readEnd(Proofs.fromBytes)
    } yield {
      OrderV3(
        senderPublicKey = sender,
        matcherPublicKey = matcher,
        assetPair = AssetPair(amountAssetId.map(ByteStr.apply), priceAssetId.map(ByteStr.apply)),
        orderType = OrderType(orderType),
        amount = amount,
        price = price,
        timestamp = timestamp,
        expiration = expiration,
        matcherFee = matcherFee,
        matcherFeeAssetId = matcherFeeAssetId.map(ByteStr.apply),
        proofs = maybeProofs.right.get
      )
    }

    makeOrder.run(0).value._2
  }
}
