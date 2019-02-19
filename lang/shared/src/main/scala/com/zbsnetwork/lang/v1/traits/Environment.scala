package com.zbsnetwork.lang.v1.traits

import com.zbsnetwork.lang.v1.traits.domain.Tx.ContractTransfer
import com.zbsnetwork.lang.v1.traits.domain._
import shapeless._

object Environment {
  type InputEntity = Tx :+: Ord :+: ContractTransfer :+: CNil
}

trait Environment {
  def height: Long
  def chainId: Byte
  def inputEntity: Environment.InputEntity
  def transactionById(id: Array[Byte]): Option[Tx]
  def transactionHeightById(id: Array[Byte]): Option[Long]
  def data(addressOrAlias: Recipient, key: String, dataType: DataType): Option[Any]
  def resolveAlias(name: String): Either[String, Recipient.Address]
  def accountBalanceOf(addressOrAlias: Recipient, assetId: Option[Array[Byte]]): Either[String, Long]
}
