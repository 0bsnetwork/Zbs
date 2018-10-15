package com.zbsplatform.matcher

import java.io.File
import java.nio.ByteBuffer
import java.util.{HashMap => JHashMap, HashSet => JHashSet}

import akka.actor.ActorSystem
import akka.persistence.serialization.Snapshot
import akka.serialization.SerializationExtension
import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Ints, Shorts}
import com.typesafe.config.{Config, ConfigFactory}
import com.zbsplatform.account.{Address, AddressScheme}
import com.zbsplatform.crypto.DigestSize
import com.zbsplatform.database._
import com.zbsplatform.db.openDB
import com.zbsplatform.matcher.api.DBUtils
import com.zbsplatform.matcher.market.{MatcherActor, OrderBookActor}
import com.zbsplatform.matcher.model.{LimitOrder, OrderBook}
import com.zbsplatform.settings.{ZbsSettings, loadConfig}
import com.zbsplatform.state.{ByteStr, EitherExt2}
import com.zbsplatform.transaction.AssetId
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order}
import com.zbsplatform.utils.ScorexLogging
import org.iq80.leveldb.DB

import scala.collection.JavaConverters._
import scala.collection.immutable.Queue
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object MatcherTool extends ScorexLogging {
  private def collectStats(db: DB): Unit = {
    log.info("Collecting stats")
    val iterator = db.iterator()
    iterator.seekToFirst()

    val result = new JHashMap[Short, Stats]

    def add(prefix: Short, e: java.util.Map.Entry[Array[Byte], Array[Byte]]): Unit = {
      result.compute(
        prefix,
        (_, maybePrev) =>
          maybePrev match {
            case null => Stats(1, e.getKey.length, e.getValue.length)
            case prev => Stats(prev.entryCount + 1, prev.totalKeySize + e.getKey.length, prev.totalValueSize + e.getValue.length)
        }
      )
    }

    try {
      while (iterator.hasNext) {
        val e = iterator.next()
        e.getKey match {
          case SK.Orders(_)                => add(100.toShort, e)
          case SK.OrdersInfo(_)            => add(101.toShort, e)
          case SK.AddressToOrders(_)       => add(102.toShort, e)
          case SK.AddressToActiveOrders(_) => add(103.toShort, e)
          case SK.AddressPortfolio(_)      => add(104.toShort, e)
          case SK.Transactions(_)          => add(104.toShort, e)
          case SK.OrdersToTxIds(_)         => add(106.toShort, e)
          case bytes =>
            val prefix = Shorts.fromByteArray(bytes.take(2))
            add(prefix, e)
        }
      }
    } finally iterator.close()

    for ((k, s) <- result.asScala) {
      println(s"$k, ${s.entryCount}, ${s.totalKeySize}, ${s.totalValueSize}")
    }
  }

  private def deleteLegacyEntries(db: DB): Unit = {
    val keysToDelete = Seq.newBuilder[Array[Byte]]

    db.iterateOver("matcher:".getBytes(UTF_8))(e => keysToDelete += e.getKey)

    db.readWrite(rw => keysToDelete.result().foreach(rw.delete))
  }

  private def recalculateReservedBalance(db: DB): Unit = {
    log.info("Recalculating reserved balances")
    val calculatedReservedBalances = new JHashMap[Address, Map[Option[AssetId], Long]]()
    val ordersToDelete             = Seq.newBuilder[ByteStr]
    var discrepancyCounter         = 0

    db.iterateOver(MatcherKeys.OrderInfoPrefix) { e =>
      val orderId   = e.extractId()
      val orderInfo = MatcherKeys.decodeOrderInfo(e.getValue)
      if (!orderInfo.status.isFinal) {
        db.get(MatcherKeys.order(orderId)) match {
          case None =>
            log.info(s"Missing order $orderId")
            ordersToDelete += orderId
          case Some(order) =>
            calculatedReservedBalances.compute(
              order.sender, { (_, prevBalances) =>
                val lo             = LimitOrder(order)
                val spendId        = order.getSpendAssetId
                val spendRemaining = lo.getRawSpendAmount - orderInfo.totalSpend(lo)
                val remainingFee   = releaseFee(lo, orderInfo.remainingFee, 0)

                val r = Option(prevBalances).fold(Map(spendId -> spendRemaining)) { prevBalances =>
                  prevBalances.updated(spendId, prevBalances.getOrElse(spendId, 0L) + spendRemaining)
                }

                r.updated(None, r.getOrElse(None, 0L) + remainingFee)
              }
            )
        }
      }
    }

    log.info("Collecting all addresses")

    val addresses = Seq.newBuilder[Address]
    db.iterateOver(Shorts.toByteArray(5)) { e =>
      val addressBytes = new Array[Byte](Address.AddressLength)
      Array.copy(e.getKey, 2, addressBytes, 0, Address.AddressLength)
      addresses += Address.fromBytes(addressBytes).explicitGet()
    }

    log.info("Loading stored reserved balances")

    val allReservedBalances = addresses.result().map(a => a -> DBUtils.reservedBalance(db, a)).toMap

    if (allReservedBalances.size != calculatedReservedBalances.size()) {
      log.info(s"Calculated balances: ${calculatedReservedBalances.size()}, stored balances: ${allReservedBalances.size}")
    }

    val corrections = Seq.newBuilder[((Address, Option[AssetId]), Long)]
    var assetsToAdd = Map.empty[Address, Set[Option[AssetId]]]

    for (address <- allReservedBalances.keySet ++ calculatedReservedBalances.keySet().asScala) {
      val calculated = calculatedReservedBalances.getOrDefault(address, Map.empty)
      val stored     = allReservedBalances.getOrElse(address, Map.empty)
      if (calculated != stored) {
        for (assetId <- calculated.keySet ++ stored.keySet) {
          val calculatedBalance = calculated.getOrElse(assetId, 0L)
          val storedBalance     = stored.getOrElse(assetId, 0L)

          if (calculatedBalance != storedBalance) {
            if (!stored.contains(assetId)) assetsToAdd += address -> (assetsToAdd.getOrElse(address, Set.empty) + assetId)

            discrepancyCounter += 1
            corrections += (address, assetId) -> calculatedBalance
          }
        }
      }
    }

    log.info(s"Found $discrepancyCounter discrepancies; writing reserved balances")

    db.readWrite { rw =>
      for ((address, newAssetIds) <- assetsToAdd) {
        val k         = MatcherKeys.openVolumeSeqNr(address)
        val currSeqNr = rw.get(k)

        rw.put(k, currSeqNr + newAssetIds.size)
        for ((assetId, i) <- newAssetIds.zipWithIndex) {
          rw.put(MatcherKeys.openVolumeAsset(address, currSeqNr + 1 + i), assetId)
        }
      }

      for (((address, assetId), value) <- corrections.result()) {
        rw.put(MatcherKeys.openVolume(address, assetId), Some(value))
      }
    }

    log.info("Completed")
  }

  private def collectActiveOrders(db: DB): Map[AssetPair, Map[ByteStr, Order]] = {
    val activeOrders = new JHashMap[AssetPair, Map[ByteStr, Order]]()
    db.iterateOver(MatcherKeys.OrderInfoPrefix) { e =>
      val info = MatcherKeys.decodeOrderInfo(e.getValue)
      if (!info.status.isFinal) {
        val orderId = e.extractId()
        db.get(MatcherKeys.order(orderId)) match {
          case Some(order) =>
            activeOrders.compute(order.assetPair, { (_, maybePrev) =>
              Option(maybePrev).fold(Map(orderId -> order))(_.updated(orderId, order))
            })
          case None =>
            log.info(s"Missing order $orderId")
        }

      }
    }

    activeOrders.asScala.toMap
  }

  private def extractPersistenceId(key: Array[Byte]): (String, Int) = (
    new String(key, 1, key.length - 5, UTF_8),
    Ints.fromByteArray(key.takeRight(4))
  )

  private def recoverOrderBooks(db: DB, matcherSnapshotsDirectory: String, config: Config, dryRun: Boolean): Unit = {
    log.info("Recovering order books")

    val system = ActorSystem("matcher-tool", config)
    val se     = SerializationExtension(system)

    val orderBookSnapshots = new JHashMap[AssetPair, (OrderBook, Int)]
    val snapshotDB         = openDB(matcherSnapshotsDirectory)
    try {
      snapshotDB.iterateOver(Array(3.toByte)) { e =>
        val (persistenceId, seqNr) = extractPersistenceId(e.getKey)
        se.deserialize(e.getValue, classOf[Snapshot]).get.data match {
          case _: MatcherActor.Snapshot => log.info("Encountered Matcher Actor snapshot")
          case OrderBookActor.Snapshot(orderBook) =>
            val pairStr = persistenceId.split("-")
            orderBookSnapshots.compute(AssetPair.createAssetPair(pairStr(0), pairStr(1)).get, { (_, v) =>
              if (v == null || v._2 < seqNr) (orderBook, seqNr) else v
            })
        }

      }

      if (orderBookSnapshots.isEmpty) {
        log.warn("No snapshots found, please check your configuration")
      } else {
        log.info(s"Collected ${orderBookSnapshots.size()} order book snapshots")

        val allOrderBooks   = orderBookSnapshots.asScala.mapValues(_._1)
        val allActiveOrders = collectActiveOrders(db)

        val snapshotsToUpdate     = new JHashMap[AssetPair, OrderBook]
        var snapshotUpdateCounter = 0
        val ordersToCancel        = Set.newBuilder[ByteStr]

        for (assetPair <- allOrderBooks.keySet ++ allActiveOrders.keySet) {
          val orderBookFromSnapshot = allOrderBooks.getOrElse(assetPair, OrderBook.empty)
          val computedActiveOrders  = allActiveOrders.getOrElse(assetPair, Map.empty)

          for (orderId <- computedActiveOrders.keySet ++ orderBookFromSnapshot.allOrderIds) {
            if (!computedActiveOrders.contains(orderId)) {
              snapshotUpdateCounter += 1
              snapshotsToUpdate.compute(
                assetPair, { (_, ob) =>
                  val currentOrderBook = Option(ob).getOrElse(orderBookFromSnapshot)
                  OrderBook
                    .cancelOrder(currentOrderBook, orderId)
                    .fold(currentOrderBook)(OrderBook.updateState(currentOrderBook, _))
                }
              )
            }
            if (!orderBookFromSnapshot.allOrderIds(orderId)) {
              ordersToCancel += orderId
            }
          }
        }
        val allOrderIdsToCancel = ordersToCancel.result().map(id => id -> DBUtils.orderInfo(db, id).copy(canceledByUser = Some(true)))
        log.info(s"Cancelling ${allOrderIdsToCancel.size} order(s)")
        db.readWrite { rw =>
          for ((id, info) <- allOrderIdsToCancel) {
            log.info(s"Cancelling order $id")
            if (!dryRun) {
              rw.put(MatcherKeys.orderInfo(id), info)
            }
          }
        }

        log.info(s"Updating ${snapshotsToUpdate.size()} snapshot(s)")
        snapshotDB.readWrite { rw =>
          for ((assetPair, orderBook) <- snapshotsToUpdate.asScala) {
            val (_, seqNr)    = orderBookSnapshots.get(assetPair)
            val snapshotBytes = se.serialize(Snapshot(OrderBookActor.Snapshot(orderBook)))
            if (!dryRun) {
              rw.put(MatcherSnapshotStore.kSnapshot(assetPair.toString, seqNr), snapshotBytes.get)
            }
          }
        }
      }
    } finally {
      log.info("Terminating actor system")
      Await.ready(system.terminate(), Duration.Inf)
      log.info("Closing snapshot store")
      snapshotDB.close()
    }
  }

  def createPairIndices(db: DB): Unit = {
    import com.zbsplatform.matcher.util.Codecs.ByteBufferExt
    val currOrderIds: JHashSet[ByteStr] = {
      val key = Shorts.toByteArray(11) // MatcherKeys.addressOrdersByPairSeqNr
      val r   = new JHashSet[ByteStr]
      db.iterateOver(key) { e =>
        val seqNr = Ints.fromByteArray(e.getValue)
        if (seqNr > 0) {
          val skip    = Address.AddressLength + 2
          val address = Address.fromBytes(e.getKey.slice(2, skip)).explicitGet()
          val pair    = ByteBuffer.wrap(e.getKey, skip, e.getKey.length - skip).getAssetPair
          (1 to seqNr).foreach { idx =>
            val key = MatcherKeys.addressOrdersByPair(address, pair, idx)
            key.parse(db.get(key.keyBytes)).foreach { orderId =>
              r.add(orderId)
            }
          }
        }
      }
      r
    }
    log.info(s"Records in new indices: ${currOrderIds.size()}")

    val orderRefs: JHashMap[(Address, AssetPair), Queue[Order.Id]] = {
      log.info("Loading orders")
      val r   = new JHashMap[(Address, AssetPair), Queue[Order.Id]]()
      val key = MatcherKeys.orderInfo(ByteStr(Array.emptyByteArray))
      db.iterateOver(key.keyBytes) { e =>
        val orderId = ByteStr(new Array[Byte](DigestSize))
        Array.copy(e.getKey, 2, orderId.arr, 0, DigestSize)
        db.get(MatcherKeys.order(orderId)).foreach { order =>
          val key = (order.sender.toAddress, order.assetPair)
          r.compute(
            key, { (_, currRefsRaw) =>
              if (currOrderIds.contains(order.id())) currRefsRaw
              else
                Option(currRefsRaw)
                  .getOrElse(Queue.empty[Order.Id])
                  .enqueue(order.id())
            }
          )
        }
      }
      r
    }

    var totalRecordsAdded = 0
    db.readWrite { rw =>
      orderRefs.forEach { (k, q) =>
        val (address, pair) = k
        val seqNrK          = MatcherKeys.addressOrdersByPairSeqNr(address, pair)

        var lastSeqNr = rw.get(seqNrK)
        var i         = 0
        q.foreach { orderId =>
          i += 1
          lastSeqNr += 1
          val k = MatcherKeys.addressOrdersByPair(address, pair, lastSeqNr)
          rw.put(k, Some(orderId))
        }

        totalRecordsAdded += lastSeqNr
        rw.put(seqNrK, lastSeqNr)
      }
    }

    log.info(s"Total records added: $totalRecordsAdded")
  }

  def writeLastActive(db: DB): Unit = {
    val lastActiveOrderIdxs: JHashMap[Address, Int] = {
      val r   = new JHashMap[Address, Int]()
      val key = Shorts.toByteArray(3) // MatcherKeys.addressOrdersSeqNr
      db.iterateOver(key) { e =>
        val addrOrdersSeqNr = Ints.fromByteArray(e.getValue)
        if (addrOrdersSeqNr > 0) {
          val address = Address.fromBytes(e.getKey.drop(2)).explicitGet()
          val activeIdxs = for {
            idx             <- (1 to db.get(MatcherKeys.addressOrdersSeqNr(address))).view
            orderSpendAsset <- db.get(MatcherKeys.addressOrders(address, idx))
            orderInfo = db.get(MatcherKeys.orderInfo(orderSpendAsset.orderId))
            if !orderInfo.status.isFinal
          } yield idx

          activeIdxs.headOption.foreach(r.put(address, _))
        }
      }
      r
    }

    log.info(s"Total addresses with active orders: ${lastActiveOrderIdxs.size()}")

    db.readWrite { rw =>
      lastActiveOrderIdxs.forEach { (address, lastIdx) =>
        rw.put(MatcherKeys.addressOldestActiveOrderSeqNr(address), Some(lastIdx))
      }
    }
  }

  def main(args: Array[String]): Unit = {
    log.info(s"OK, engine start")

    val userConfig   = args.headOption.fold(ConfigFactory.empty())(f => ConfigFactory.parseFile(new File(f)))
    val actualConfig = loadConfig(userConfig)
    val settings     = ZbsSettings.fromConfig(actualConfig)
    val db           = openDB(settings.matcherSettings.dataDir)

    AddressScheme.current = new AddressScheme {
      override val chainId: Byte = settings.blockchainSettings.addressSchemeCharacter.toByte
    }

    args(1) match {
      case "stats" => collectStats(db)
      case "ao" =>
        if (args.length == 5) {
          val pair = AssetPair.createAssetPair(args(3), args(4)).get
          val o    = DBUtils.ordersByAddressAndPair(db, Address.fromString(args(2)).explicitGet(), pair, Int.MaxValue)
          println(o.mkString("\n"))
        } else {
          val o = DBUtils.ordersByAddress(db, Address.fromString(args(2)).explicitGet(), activeOnly = false, Int.MaxValue)
          println(o.mkString("\n"))
        }
      case "cb" => recalculateReservedBalance(db)
      case "rb" =>
        for ((assetId, balance) <- DBUtils.reservedBalance(db, Address.fromString(args(2)).explicitGet())) {
          log.info(s"${AssetPair.assetIdStr(assetId)}: $balance")
        }
      case "ddd" =>
        log.warn("DELETING LEGACY ENTRIES")
        deleteLegacyEntries(db)
        log.info("Finished deleting legacy entries")
      case "compact" =>
        log.info("Compacting database")
        db.compactRange(null, null)
        log.info("Compaction complete")
      case "recover-orderbooks" =>
        val dryRun = args.length == 3 && args(2) == "--dry-run"
        recoverOrderBooks(db, settings.matcherSettings.snapshotsDataDir, actualConfig, dryRun)
      case "create-pair-indices" =>
        log.info("Creating pair indices")
        createPairIndices(db)
        log.info("Completed")
      case "write-last-active" =>
        log.info("Writing last active indices")
        writeLastActive(db)
        log.info("Completed")
      case _ =>
    }

    db.close()
  }

  case class Stats(entryCount: Long, totalKeySize: Long, totalValueSize: Long)

  class SK[A](suffix: String, extractor: Array[Byte] => Option[A]) {
    val keyBytes = ("matcher:" + suffix + ":").getBytes(UTF_8)
    def unapply(bytes: Array[Byte]): Option[A] = {
      val (prefix, suffix) = bytes.splitAt(keyBytes.length)
      if (prefix.sameElements(keyBytes)) extractor(suffix) else None
    }
  }

  object SK {
    def apply[A](suffix: String, extractor: Array[Byte] => Option[A]) = new SK(suffix, extractor)

    private def byteStr(b: Array[Byte]) = ByteStr.decodeBase58(new String(b, UTF_8)).toOption
    private def addr(b: Array[Byte])    = Address.fromString(new String(b, UTF_8)).toOption

    val Orders                = SK("orders", byteStr)
    val OrdersInfo            = SK("infos", byteStr)
    val AddressToOrders       = SK("addr-orders", addr)
    val AddressToActiveOrders = SK("a-addr-orders", addr)
    val AddressPortfolio      = SK("portfolios", addr)
    val Transactions          = SK("transactions", byteStr)
    val OrdersToTxIds         = SK("ord-to-tx-ids", byteStr)
  }

  /**
    * @return How much reserved fee we should return during this update
    */
  private def releaseFee(totalReceiveAmount: Long, matcherFee: Long, prevRemaining: Long, updatedRemaining: Long): Long = {
    val executedBefore = matcherFee - prevRemaining
    val restReserved   = math.max(matcherFee - totalReceiveAmount - executedBefore, 0L)

    val executed = prevRemaining - updatedRemaining
    math.min(executed, restReserved)
  }

  private def releaseFee(lo: LimitOrder, prevRemaining: Long, updatedRemaining: Long): Long = {
    if (lo.rcvAsset == lo.feeAsset) releaseFee(lo.getReceiveAmount, lo.order.matcherFee, prevRemaining, updatedRemaining)
    else prevRemaining - updatedRemaining
  }
}
