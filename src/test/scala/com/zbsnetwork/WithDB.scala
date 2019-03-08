package com.zbsnetwork

import java.nio.file.Files

import com.zbsnetwork.account.Address
import com.zbsnetwork.db.LevelDBFactory
import com.zbsnetwork.transaction.AssetId
import com.zbsnetwork.utils.Implicits.SubjectOps
import monix.reactive.subjects.Subject
import org.iq80.leveldb.{DB, Options}
import org.scalatest.{BeforeAndAfterEach, TestSuite}

trait WithDB extends BeforeAndAfterEach {
  this: TestSuite =>

  private val path                  = Files.createTempDirectory("lvl").toAbsolutePath
  private var currentDBInstance: DB = _

  def db: DB = currentDBInstance

  protected val ignoreSpendableBalanceChanged: Subject[(Address, Option[AssetId]), (Address, Option[AssetId])] = Subject.empty

  override def beforeEach(): Unit = {
    currentDBInstance = LevelDBFactory.factory.open(path.toFile, new Options().createIfMissing(true))
    super.beforeEach()
  }

  override def afterEach(): Unit =
    try {
      super.afterEach()
      db.close()
    } finally {
      TestHelpers.deleteRecursively(path)
    }
}
