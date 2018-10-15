package com

import com.zbsplatform.block.Block
import com.zbsplatform.settings.ZbsSettings
import com.zbsplatform.state.{ByteStr, NG}
import com.zbsplatform.transaction.ValidationError.GenericError
import com.zbsplatform.transaction.{BlockchainUpdater, ValidationError}
import com.zbsplatform.utils.ScorexLogging

package object zbsplatform extends ScorexLogging {
  private def checkOrAppend(block: Block, blockchainUpdater: BlockchainUpdater with NG): Either[ValidationError, Unit] = {
    if (blockchainUpdater.isEmpty) {
      blockchainUpdater.processBlock(block).right.map { _ =>
        log.info(s"Genesis block ${blockchainUpdater.blockHeaderAndSize(1).get._1} has been added to the state")
      }
    } else {
      val existingGenesisBlockId: Option[ByteStr] = blockchainUpdater.blockHeaderAndSize(1).map(_._1.signerData.signature)
      Either.cond(existingGenesisBlockId.fold(false)(_ == block.uniqueId),
                  (),
                  GenericError("Mismatched genesis blocks in configuration and blockchain"))
    }
  }

  def checkGenesis(settings: ZbsSettings, blockchainUpdater: BlockchainUpdater with NG): Unit = {
    Block.genesis(settings.blockchainSettings.genesisSettings).flatMap(b => checkOrAppend(b, blockchainUpdater)).left.foreach { e =>
      log.error("INCORRECT NODE CONFIGURATION!!! NODE STOPPED BECAUSE OF THE FOLLOWING ERROR:")
      log.error(e.toString)
      com.zbsplatform.utils.forceStopApplication()
    }
  }
}
