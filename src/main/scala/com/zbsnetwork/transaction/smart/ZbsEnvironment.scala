package com.zbsplatform.transaction.smart

import com.zbsplatform.lang.v1.traits.Recipient.{Address, Alias}
import com.zbsplatform.lang.v1.traits.{DataType, Environment, Recipient, Tx => ContractTransaction}
import com.zbsplatform.state._
import monix.eval.Coeval
import scodec.bits.ByteVector
import com.zbsplatform.account.AddressOrAlias
import com.zbsplatform.transaction.Transaction

class ZbsEnvironment(nByte: Byte, tx: Coeval[Transaction], h: Coeval[Int], blockchain: Blockchain) extends Environment {
  override def height: Int = h()

  override def transaction: ContractTransaction = RealTransactionWrapper(tx())

  override def transactionById(id: Array[Byte]): Option[ContractTransaction] =
    blockchain
      .transactionInfo(ByteStr(id))
      .map(_._2)
      .map(RealTransactionWrapper(_))

  override def data(recipient: Recipient, key: String, dataType: DataType): Option[Any] = {
    for {
      address <- recipient match {
        case Address(bytes) =>
          com.zbsplatform.account.Address
            .fromBytes(bytes.toArray)
            .toOption
        case Alias(name) =>
          com.zbsplatform.account.Alias
            .buildWithCurrentNetworkByte(name)
            .flatMap(blockchain.resolveAlias)
            .toOption
      }
      data <- blockchain
        .accountData(address, key)
        .map((_, dataType))
        .flatMap {
          case (IntegerDataEntry(_, value), DataType.Long)     => Some(value)
          case (BooleanDataEntry(_, value), DataType.Boolean)  => Some(value)
          case (BinaryDataEntry(_, value), DataType.ByteArray) => Some(ByteVector(value.arr))
          case (StringDataEntry(_, value), DataType.String)    => Some(value)
          case _                                               => None
        }
    } yield data
  }
  override def resolveAlias(name: String): Either[String, Recipient.Address] =
    blockchain
      .resolveAlias(com.zbsplatform.account.Alias.buildWithCurrentNetworkByte(name).explicitGet())
      .left
      .map(_.toString)
      .right
      .map(a => Recipient.Address(ByteVector(a.bytes.arr)))

  override def networkByte: Byte = nByte

  override def accountBalanceOf(addressOrAlias: Recipient, maybeAssetId: Option[Array[Byte]]): Either[String, Long] = {
    (for {
      aoa <- addressOrAlias match {
        case Address(bytes) => AddressOrAlias.fromBytes(bytes.toArray, position = 0).map(_._1)
        case Alias(name)    => com.zbsplatform.account.Alias.buildWithCurrentNetworkByte(name)
      }
      address <- blockchain.resolveAlias(aoa)
      balance = blockchain.balance(address, maybeAssetId.map(ByteStr(_)))
    } yield balance).left.map(_.toString)
  }
  override def transactionHeightById(id: Array[Byte]): Option[Int] =
    blockchain.transactionHeight(ByteStr(id))
}
