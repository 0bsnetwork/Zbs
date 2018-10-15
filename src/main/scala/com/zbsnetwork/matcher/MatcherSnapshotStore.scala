package com.zbsplatform.matcher

import java.nio.ByteBuffer

import akka.persistence._
import akka.persistence.serialization._
import akka.persistence.snapshot.SnapshotStore
import akka.serialization.SerializationExtension
import com.google.common.base.Charsets.UTF_8
import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.Config
import com.zbsplatform.database._
import com.zbsplatform.db.openDB
import com.zbsplatform.utils.ScorexLogging
import org.iq80.leveldb.ReadOptions

import scala.concurrent.Future
import scala.util._

class MatcherSnapshotStore(config: Config) extends SnapshotStore {
  import MatcherSnapshotStore._

  private val streamDispatcher = context.system.dispatchers.lookup(config.getString("stream-dispatcher"))

  private val serializationExtension = SerializationExtension(context.system)

  private val writableDB = openDB(config.getString("dir"))

  private def readOnly[A](f: ReadOnlyDB => A): A = {
    val s = writableDB.getSnapshot
    try f(new ReadOnlyDB(writableDB, new ReadOptions().snapshot(s)))
    finally s.close()
  }

  private def readWrite[A](f: RW => A): A = {
    val rw = new RW(writableDB)
    try f(rw)
    finally rw.close()
  }

  private var pendingActions = Map.empty[String, Future[_]]

  private def nextAction[A](persistenceId: String, f: => A): Future[A] = {
    val newAction = pendingActions
      .getOrElse(persistenceId, Future.unit)
      .transform { _ =>
        Try(f)
      }(streamDispatcher)
    pendingActions += persistenceId -> newAction
    newAction
  }

  override def loadAsync(persistenceId: String, criteria: SnapshotSelectionCriteria): Future[Option[SelectedSnapshot]] =
    nextAction(
      persistenceId,
      readOnly { db =>
        db.get(kSMHistory(persistenceId))
          .find(seqNr => db.get(kSM(persistenceId, seqNr)).matches(criteria))
          .map { seqNr =>
            val sm = db.get(kSM(persistenceId, seqNr))
            SelectedSnapshot(SnapshotMetadata(persistenceId, sm.seqNr, sm.ts), deserialize(db.get(kSnapshot(persistenceId, seqNr))).data)
          }
      }
    )

  override def saveAsync(metadata: SnapshotMetadata, snapshot: Any): Future[Unit] =
    nextAction(
      metadata.persistenceId,
      save(metadata, snapshot)
    )

  override def deleteAsync(metadata: SnapshotMetadata): Future[Unit] =
    nextAction(
      metadata.persistenceId,
      readWrite { db =>
        val historyKey = kSMHistory(metadata.persistenceId)
        val history    = db.get(historyKey)
        history
          .find(seqNr => db.get(kSM(metadata.persistenceId, seqNr)).matches(metadata))
          .foreach { seqNr =>
            db.put(historyKey, history.filterNot(_ == seqNr))
            db.delete(kSM(metadata.persistenceId, seqNr))
            db.delete(kSnapshot(metadata.persistenceId, seqNr))
          }
      }
    )

  override def deleteAsync(persistenceId: String, criteria: SnapshotSelectionCriteria): Future[Unit] =
    nextAction(
      persistenceId,
      readWrite { db =>
        val history            = db.get(kSMHistory(persistenceId))
        val (toDelete, toKeep) = history.map(seqNr => seqNr -> db.get(kSM(persistenceId, seqNr))).partition { case (_, sm) => sm.matches(criteria) }
        db.put(kSMHistory(persistenceId), toKeep.map(_._1).sorted.reverse)
        for ((seqNr, _) <- toDelete) {
          db.delete(kSM(persistenceId, seqNr))
          db.delete(kSnapshot(persistenceId, seqNr))
        }
      }
    )

  override def receivePluginInternal: Receive = {
    case _: SaveSnapshotSuccess    ⇒ // ignore
    case _: SaveSnapshotFailure    ⇒ // ignore
    case _: DeleteSnapshotsSuccess ⇒ // ignore
    case _: DeleteSnapshotsFailure ⇒ // ignore
  }

  protected def save(metadata: SnapshotMetadata, snapshot: Any): Unit =
    readWrite { rw =>
      val historyKey      = kSMHistory(metadata.persistenceId)
      val previousHistory = rw.get(historyKey)
      val nextId          = previousHistory.headOption.getOrElse(0) + 1
      val nextHistory     = nextId +: previousHistory
      rw.put(historyKey, nextHistory)
      rw.put(kSM(metadata.persistenceId, nextId), SM(metadata.sequenceNr, metadata.timestamp))
      rw.put(kSnapshot(metadata.persistenceId, nextId), serialize(snapshot))
    }

  protected def deserialize(input: Array[Byte]): Snapshot =
    serializationExtension.deserialize(input, classOf[Snapshot]).get

  private def serialize(s: Any) = serializationExtension.serialize(Snapshot(s)).get
}

object MatcherSnapshotStore extends ScorexLogging {
  case class SM(seqNr: Long, ts: Long) {
    def matches(criteria: SnapshotSelectionCriteria): Boolean =
      criteria.minSequenceNr <= seqNr && seqNr <= criteria.maxSequenceNr &&
        criteria.minTimestamp <= ts && ts <= criteria.maxTimestamp

    def matches(metadata: SnapshotMetadata): Boolean =
      seqNr == metadata.sequenceNr && (metadata.timestamp == 0 || ts == metadata.timestamp)
  }

  private def readSnapshotMetadata(b: Array[Byte]) = {
    val bb = ByteBuffer.wrap(b)
    SM(bb.getLong, bb.getLong)
  }

  private def writeSnapshotMetadata(sm: SM) =
    ByteBuffer.allocate(16).putLong(sm.seqNr).putLong(sm.ts).array()

  private def kSMHistory(persistenceId: String) = Key[Seq[Int]](Bytes.concat(Array(1: Byte), persistenceId.getBytes(UTF_8)), readIntSeq, writeIntSeq)
  private def kSM(persistenceId: String, seqNr: Int) =
    Key[SM](Bytes.concat(Array(2: Byte), persistenceId.getBytes(UTF_8), Ints.toByteArray(seqNr)), readSnapshotMetadata, writeSnapshotMetadata)
  def kSnapshot(persistenceId: String, seqNr: Int) =
    Key[Array[Byte]](Bytes.concat(Array(3: Byte), persistenceId.getBytes(UTF_8), Ints.toByteArray(seqNr)), identity, identity)
}
