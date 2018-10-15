package com.zbsplatform.matcher.model

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.SerializerProvider
import com.fasterxml.jackson.databind.annotation.JsonSerialize
import com.fasterxml.jackson.databind.ser.std.StdSerializer
import com.zbsplatform.matcher.api.JsonSerializer
import com.zbsplatform.transaction.assets.exchange.AssetPair
import com.zbsplatform.utils.NTP

@JsonSerialize(using = classOf[OrderBookResult.Serializer])
case class OrderBookResult(timestamp: Long, pair: AssetPair, bids: Seq[LevelAgg], asks: Seq[LevelAgg])

object OrderBookResult {

  def empty(pair: AssetPair) = OrderBookResult(NTP.correctedTime(), pair, Seq.empty, Seq.empty)

  def toJson(x: OrderBookResult): String = JsonSerializer.serialize(x)

  class Serializer extends StdSerializer[OrderBookResult](classOf[OrderBookResult]) {
    override def serialize(x: OrderBookResult, j: JsonGenerator, serializerProvider: SerializerProvider): Unit = {
      j.writeStartObject()

      j.writeNumberField("timestamp", x.timestamp)

      j.writeFieldName("pair")
      j.writeStartObject()
      j.writeStringField("amountAsset", x.pair.amountAssetStr)
      j.writeStringField("priceAsset", x.pair.priceAssetStr)
      j.writeEndObject()

      j.writeArrayFieldStart("bids")
      x.bids.foreach(j.writeObject)
      j.writeEndArray()

      j.writeArrayFieldStart("asks")
      x.asks.foreach(j.writeObject)
      j.writeEndArray()

      j.writeEndObject()
    }
  }
}
