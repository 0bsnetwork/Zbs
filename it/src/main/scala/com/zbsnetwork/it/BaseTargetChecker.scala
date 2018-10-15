package com.zbsplatform.it

import com.typesafe.config.ConfigFactory.{defaultApplication, defaultReference}
import com.zbsplatform.consensus.PoSSelector
import com.zbsplatform.db.openDB
import com.zbsplatform.history.StorageFactory
import com.zbsplatform.settings._
import com.zbsplatform.state.{ByteStr, EitherExt2}
import net.ceedubs.ficus.Ficus._
import com.zbsplatform.account.PublicKeyAccount
import com.zbsplatform.utils.NTP
import com.zbsplatform.block.Block

object BaseTargetChecker {
  def main(args: Array[String]): Unit = {
    val docker = Docker(getClass)
    val sharedConfig = docker.genesisOverride
      .withFallback(docker.configTemplate)
      .withFallback(defaultApplication())
      .withFallback(defaultReference())
      .resolve()
    val settings     = ZbsSettings.fromConfig(sharedConfig)
    val genesisBlock = Block.genesis(settings.blockchainSettings.genesisSettings).explicitGet()
    val db           = openDB("/tmp/tmp-db")
    val bu           = StorageFactory(settings, db, NTP)
    val pos          = new PoSSelector(bu, settings.blockchainSettings)
    bu.processBlock(genesisBlock)

    NodeConfigs.Default.map(_.withFallback(sharedConfig)).collect {
      case cfg if cfg.as[Boolean]("zbs.miner.enable") =>
        val account   = PublicKeyAccount(cfg.as[ByteStr]("public-key").arr)
        val address   = account.toAddress
        val balance   = bu.balance(address, None)
        val consensus = genesisBlock.consensusData
        val timeDelay = pos
          .getValidBlockDelay(bu.height, account.publicKey, consensus.baseTarget, balance)
          .explicitGet()

        f"$address: ${timeDelay * 1e-3}%10.3f s"
    }

    docker.close()
  }
}
