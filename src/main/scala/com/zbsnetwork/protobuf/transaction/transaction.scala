package com.zbsnetwork.protobuf

package object transaction {
  type PBOrder = com.zbsnetwork.protobuf.transaction.ExchangeTransactionData.Order
  val PBOrder = com.zbsnetwork.protobuf.transaction.ExchangeTransactionData.Order

  type VanillaOrder = com.zbsnetwork.transaction.assets.exchange.Order
  val VanillaOrder = com.zbsnetwork.transaction.assets.exchange.Order

  type PBTransaction = com.zbsnetwork.protobuf.transaction.Transaction
  val PBTransaction = com.zbsnetwork.protobuf.transaction.Transaction

  type PBSignedTransaction = com.zbsnetwork.protobuf.transaction.SignedTransaction
  val PBSignedTransaction = com.zbsnetwork.protobuf.transaction.SignedTransaction

  type VanillaTransaction = com.zbsnetwork.transaction.Transaction
  val VanillaTransaction = com.zbsnetwork.transaction.Transaction

  type VanillaSignedTransaction = com.zbsnetwork.transaction.SignedTransaction

  type VanillaAssetId = com.zbsnetwork.transaction.AssetId
}
