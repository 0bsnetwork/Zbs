package com.zbsnetwork.db

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.zbsnetwork.account.Address
import com.zbsnetwork.database.LevelDBWriter
import com.zbsnetwork.history.Domain
import com.zbsnetwork.settings.{FunctionalitySettings, ZbsSettings, loadConfig}
import com.zbsnetwork.state.{Blockchain, BlockchainUpdaterImpl}
import com.zbsnetwork.transaction.AssetId
import com.zbsnetwork.utils.Implicits.SubjectOps
import com.zbsnetwork.{NTPTime, TestHelpers}
import monix.reactive.subjects.Subject
import org.scalatest.Suite

trait WithState extends DBCacheSettings {
  protected val ignoreSpendableBalanceChanged: Subject[(Address, Option[AssetId]), (Address, Option[AssetId])] = Subject.empty
  protected def withState[A](fs: FunctionalitySettings)(f: Blockchain => A): A = {
    val path = Files.createTempDirectory("leveldb-test")
    val db   = openDB(path.toAbsolutePath.toString)
    try f(new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, maxCacheSize, 2000, 120 * 60 * 1000))
    finally {
      db.close()
      TestHelpers.deleteRecursively(path)
    }
  }

  def withStateAndHistory(fs: FunctionalitySettings)(test: Blockchain => Any): Unit = withState(fs)(test)
}

trait WithDomain extends WithState with NTPTime {
  _: Suite =>

  def withDomain[A](settings: ZbsSettings = ZbsSettings.fromConfig(loadConfig(ConfigFactory.load())))(test: Domain => A): A = {
    try withState(settings.blockchainSettings.functionalitySettings) { blockchain =>
      val bcu = new BlockchainUpdaterImpl(blockchain, ignoreSpendableBalanceChanged, settings, ntpTime)
      try test(Domain(bcu))
      finally bcu.shutdown()
    } finally {}
  }
}
