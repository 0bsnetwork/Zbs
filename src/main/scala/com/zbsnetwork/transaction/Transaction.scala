package com.zbsnetwork.transaction

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.state._
import monix.eval.Coeval
import com.zbsnetwork.serialization.{BytesSerializable, JsonSerializable}

trait Transaction extends BytesSerializable with JsonSerializable {
  val id: Coeval[ByteStr]

  def builder: TransactionParser
  def assetFee: (Option[AssetId], Long)
  def timestamp: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _               => false
  }

  override def hashCode(): Int = id().hashCode()

  val bodyBytes: Coeval[Array[Byte]]
  def checkedAssets(): Seq[AssetId] = Seq.empty
}

object Transaction {

  type Type = Byte

  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (Some(asset), fee) =>
        Portfolio(balance = 0, lease = LeaseBalance.empty, assets = Map(asset -> fee))
      case (None, fee) => Portfolio(balance = fee, lease = LeaseBalance.empty, assets = Map.empty)
    }
  }

}
