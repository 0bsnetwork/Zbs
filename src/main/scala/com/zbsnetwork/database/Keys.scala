package com.zbsnetwork.database

import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Longs}
import com.zbsnetwork.account.{Address, Alias}
import com.zbsnetwork.block.BlockHeader
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.smart.script.{Script, ScriptReader}
import com.zbsnetwork.transaction.{Transaction, TransactionParsers}

object Keys {
  import KeyHelpers._

  val version: Key[Int]               = intKey("version", 0, default = 1)
  val height: Key[Int]                = intKey("height", 1)
  def score(height: Int): Key[BigInt] = Key("score", h(2, height), Option(_).fold(BigInt(0))(BigInt(_)), _.toByteArray)

  def heightOf(blockId: ByteStr): Key[Option[Int]] = Key.opt[Int]("height-of", hash(4, blockId), Ints.fromByteArray, Ints.toByteArray)

  def zbsBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("zbs-balance-history", 5, addressId.toByteArray)
  def zbsBalance(addressId: BigInt)(height: Int): Key[Long] =
    Key("zbs-balance", hAddr(6, height, addressId), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetList(addressId: BigInt): Key[Set[ByteStr]] =
    Key("asset-list", addr(7, addressId), readTxIds(_).toSet, assets => writeTxIds(assets.toSeq))
  def assetBalanceHistory(addressId: BigInt, assetId: ByteStr): Key[Seq[Int]] =
    historyKey("asset-balance-history", 8, addressId.toByteArray ++ assetId.arr)
  def assetBalance(addressId: BigInt, assetId: ByteStr)(height: Int): Key[Long] =
    Key("asset-balance", hBytes(9, height, addressId.toByteArray ++ assetId.arr), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetInfoHistory(assetId: ByteStr): Key[Seq[Int]] = historyKey("asset-info-history", 10, assetId.arr)
  def assetInfo(assetId: ByteStr)(height: Int): Key[AssetInfo] =
    Key("asset-info", hBytes(11, height, assetId.arr), readAssetInfo, writeAssetInfo)

  def leaseBalanceHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("lease-balance-history", 12, addressId.toByteArray)
  def leaseBalance(addressId: BigInt)(height: Int): Key[LeaseBalance] =
    Key("lease-balance", hAddr(13, height, addressId), readLeaseBalance, writeLeaseBalance)
  def leaseStatusHistory(leaseId: ByteStr): Key[Seq[Int]] = historyKey("lease-status-history", 14, leaseId.arr)
  def leaseStatus(leaseId: ByteStr)(height: Int): Key[Boolean] =
    Key("lease-status", hBytes(15, height, leaseId.arr), _(0) == 1, active => Array[Byte](if (active) 1 else 0))

  def filledVolumeAndFeeHistory(orderId: ByteStr): Key[Seq[Int]] = historyKey("filled-volume-and-fee-history", 16, orderId.arr)
  def filledVolumeAndFee(orderId: ByteStr)(height: Int): Key[VolumeAndFee] =
    Key("filled-volume-and-fee", hBytes(17, height, orderId.arr), readVolumeAndFee, writeVolumeAndFee)

  // 19, 20 were never used

  def changedAddresses(height: Int): Key[Seq[BigInt]] = Key("changed-addresses", h(21, height), readBigIntSeq, writeBigIntSeq)

  def addressIdOfAlias(alias: Alias): Key[Option[BigInt]] = Key.opt("address-id-of-alias", bytes(23, alias.bytes.arr), BigInt(_), _.toByteArray)

  val lastAddressId: Key[Option[BigInt]] = Key.opt("last-address-id", Array[Byte](0, 24), BigInt(_), _.toByteArray)

  def addressId(address: Address): Key[Option[BigInt]] = Key.opt("address-id", bytes(25, address.bytes.arr), BigInt(_), _.toByteArray)
  def idToAddress(id: BigInt): Key[Address]            = Key("id-to-address", bytes(26, id.toByteArray), Address.fromBytes(_).explicitGet(), _.bytes.arr)

  def addressScriptHistory(addressId: BigInt): Key[Seq[Int]] = historyKey("address-script-history", 27, addressId.toByteArray)
  def addressScript(addressId: BigInt)(height: Int): Key[Option[Script]] =
    Key.opt("address-script", hAddr(28, height, addressId), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)

  val approvedFeatures: Key[Map[Short, Int]]  = Key("approved-features", Array[Byte](0, 29), readFeatureMap, writeFeatureMap)
  val activatedFeatures: Key[Map[Short, Int]] = Key("activated-features", Array[Byte](0, 30), readFeatureMap, writeFeatureMap)

  def dataKeyChunkCount(addressId: BigInt): Key[Int] =
    Key("data-key-chunk-count", addr(31, addressId), Option(_).fold(0)(Ints.fromByteArray), Ints.toByteArray)
  def dataKeyChunk(addressId: BigInt, chunkNo: Int): Key[Seq[String]] =
    Key("data-key-chunk", addr(32, addressId) ++ Ints.toByteArray(chunkNo), readStrings, writeStrings)

  def dataHistory(addressId: BigInt, key: String): Key[Seq[Int]] = historyKey("data-history", 33, addressId.toByteArray ++ key.getBytes(UTF_8))
  def data(addressId: BigInt, key: String)(height: Int): Key[Option[DataEntry[_]]] =
    Key.opt("data", hBytes(34, height, addressId.toByteArray ++ key.getBytes(UTF_8)), DataEntry.parseValue(key, _, 0)._1, _.valueBytes)

  def sponsorshipHistory(assetId: ByteStr): Key[Seq[Int]] = historyKey("sponsorship-history", 35, assetId.arr)
  def sponsorship(assetId: ByteStr)(height: Int): Key[SponsorshipValue] =
    Key("sponsorship", hBytes(36, height, assetId.arr), readSponsorship, writeSponsorship)

  val addressesForZbsSeqNr: Key[Int]                = intKey("addresses-for-zbs-seq-nr", 37)
  def addressesForZbs(seqNr: Int): Key[Seq[BigInt]] = Key("addresses-for-zbs", h(38, seqNr), readBigIntSeq, writeBigIntSeq)

  def addressesForAssetSeqNr(assetId: ByteStr): Key[Int] = bytesSeqNr("addresses-for-asset-seq-nr", 39, assetId.arr)
  def addressesForAsset(assetId: ByteStr, seqNr: Int): Key[Seq[BigInt]] =
    Key("addresses-for-asset", hBytes(40, seqNr, assetId.arr), readBigIntSeq, writeBigIntSeq)

  val AliasIsDisabledPrefix: Short = 43
  def aliasIsDisabled(alias: Alias): Key[Boolean] =
    Key("alias-is-disabled", bytes(AliasIsDisabledPrefix, alias.bytes.arr), Option(_).exists(_(0) == 1), if (_) Array[Byte](1) else Array[Byte](0))

  val carryFeeHistory: Key[Seq[Int]]   = historyKey("carry-fee-history", 44, Array())
  def carryFee(height: Int): Key[Long] = Key("carry-fee", h(45, height), Option(_).fold(0L)(Longs.fromByteArray), Longs.toByteArray)

  def assetScriptHistory(assetId: ByteStr): Key[Seq[Int]] = historyKey("asset-script-history", 46, assetId.arr)
  def assetScript(assetId: ByteStr)(height: Int): Key[Option[Script]] =
    Key.opt("asset-script", hBytes(47, height, assetId.arr), ScriptReader.fromBytes(_).explicitGet(), _.bytes().arr)
  def assetScriptPresent(assetId: ByteStr)(height: Int): Key[Option[Unit]] =
    Key.opt("asset-script", hBytes(47, height, assetId.arr), (_ => ()), (_ => Array[Byte]()))

  val safeRollbackHeight: Key[Int] = intKey("safe-rollback-height", 48)

  def changedDataKeys(height: Int, addressId: BigInt): Key[Seq[String]] =
    Key("changed-data-keys", hAddr(49, height, addressId), readStrings, writeStrings)

  val BlockHeaderPrefix: Short = 50

  def blockHeaderAndSizeAt(height: Height): Key[Option[(BlockHeader, Int)]] =
    Key.opt("block-header-at-height", h(BlockHeaderPrefix, height), readBlockHeaderAndSize, writeBlockHeaderAndSize)

  def blockHeaderBytesAt(height: Height): Key[Option[Array[Byte]]] =
    Key.opt(
      "block-header-bytes-at-height",
      h(BlockHeaderPrefix, height),
      _.drop(4),
      _ => throw new Exception("Key \"block-header-bytes-at-height\" - is read only!")
    )

  val TransactionInfoPrefix: Short = 51
  def transactionAt(height: Height, n: TxNum): Key[Option[Transaction]] =
    Key.opt[Transaction](
      "nth-transaction-info-at-height",
      hNum(TransactionInfoPrefix, height, n),
      data => TransactionParsers.parseBytes(data).get,
      _.bytes()
    )

  def transactionBytesAt(height: Height, n: TxNum): Key[Option[Array[Byte]]] =
    Key.opt(
      "nth-transaction-info-bytes-at-height",
      hNum(TransactionInfoPrefix, height, n),
      identity,
      identity
    )

  val AddressTransactionSeqNrPrefix: Short = 52
  def addressTransactionSeqNr(addressId: AddressId): Key[Int] =
    bytesSeqNr("address-transaction-seq-nr", AddressTransactionSeqNrPrefix, addressId.toByteArray)

  val AddressTransactionHNPrefix: Short = 53
  def addressTransactionHN(addressId: AddressId, seqNr: Int): Key[Option[(Height, Seq[(Byte, TxNum)])]] =
    Key.opt(
      "address-transaction-height-type-and-nums",
      hBytes(AddressTransactionHNPrefix, seqNr, addressId.toByteArray),
      readTransactionHNSeqAndType,
      writeTransactionHNSeqAndType
    )

  val TransactionHeightNumByIdPrefix: Short = 54
  def transactionHNById(txId: TransactionId): Key[Option[(Height, TxNum)]] =
    Key.opt(
      "transaction-height-and-nums-by-id",
      bytes(TransactionHeightNumByIdPrefix, txId.arr),
      readTransactionHN,
      writeTransactionHN
    )

}
