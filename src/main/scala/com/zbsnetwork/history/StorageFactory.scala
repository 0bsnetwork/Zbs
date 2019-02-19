package com.zbsnetwork.history

import com.zbsnetwork.account.Address
import com.zbsnetwork.database.{DBExt, Keys, LevelDBWriter}
import com.zbsnetwork.settings.ZbsSettings
import com.zbsnetwork.state.{BlockchainUpdaterImpl, NG}
import com.zbsnetwork.transaction.BlockchainUpdater
import com.zbsnetwork.utils.{ScorexLogging, Time, UnsupportedFeature, forceStopApplication}
import monix.reactive.Observer
import org.iq80.leveldb.DB

object StorageFactory extends ScorexLogging {
  private val StorageVersion = 3

  def apply(settings: ZbsSettings, db: DB, time: Time, portfolioChanged: Observer[Address]): BlockchainUpdater with NG = {
    checkVersion(db)
    val levelDBWriter = new LevelDBWriter(
      db,
      portfolioChanged,
      settings.blockchainSettings.functionalitySettings,
      settings.maxCacheSize,
      settings.maxRollbackDepth,
      settings.rememberBlocks.toMillis
    )
    new BlockchainUpdaterImpl(levelDBWriter, portfolioChanged, settings, time)
  }

  private def checkVersion(db: DB): Unit = db.readWrite { rw =>
    val version = rw.get(Keys.version)
    val height  = rw.get(Keys.height)
    if (version != StorageVersion) {
      if (height == 0) {
        // The storage is empty, set current version
        rw.put(Keys.version, StorageVersion)
      } else {
        // Here we've detected that the storage is not empty and doesn't contain version
        log.error(
          s"Storage version $version is not compatible with expected version $StorageVersion! Please, rebuild node's state, use import or sync from scratch.")
        log.error("FOR THIS REASON THE NODE STOPPED AUTOMATICALLY")
        forceStopApplication(UnsupportedFeature)
      }
    }
  }
}
