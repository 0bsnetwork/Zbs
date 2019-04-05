package com.zbsnetwork.mining

import cats.data.NonEmptyList
import com.zbsnetwork.block.Block
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.features.FeatureProvider._
import com.zbsnetwork.settings.MinerSettings
import com.zbsnetwork.state.Blockchain

case class MiningConstraints(total: MiningConstraint, keyBlock: MiningConstraint, micro: MiningConstraint)

object MiningConstraints {
  val MaxScriptRunsInBlock              = 100
  private val ClassicAmountOfTxsInBlock = 100
  private val MaxTxsSizeInBytes         = 1 * 1024 * 1024 // 1 megabyte

  def apply(blockchain: Blockchain, height: Int, minerSettings: Option[MinerSettings] = None): MiningConstraints = {
    val activatedFeatures     = blockchain.activatedFeaturesAt(height)
    val isNgEnabled           = activatedFeatures.contains(BlockchainFeatures.NG.id)
    val isMassTransferEnabled = activatedFeatures.contains(BlockchainFeatures.MassTransfer.id)
    val isScriptEnabled       = activatedFeatures.contains(BlockchainFeatures.SmartAccounts.id)

    val total: MiningConstraint =
      if (isMassTransferEnabled) OneDimensionalMiningConstraint(MaxTxsSizeInBytes, TxEstimators.sizeInBytes, "MaxTxsSizeInBytes")
      else {
        val maxTxs = if (isNgEnabled) Block.MaxTransactionsPerBlockVer3 else ClassicAmountOfTxsInBlock
        OneDimensionalMiningConstraint(maxTxs, TxEstimators.one, "MaxTxs")
      }

    new MiningConstraints(
      total =
        if (isScriptEnabled)
          MultiDimensionalMiningConstraint(
            NonEmptyList.of(OneDimensionalMiningConstraint(MaxScriptRunsInBlock, TxEstimators.scriptRunNumber, "MaxScriptRunsInBlock"), total))
        else total,
      keyBlock =
        if (isNgEnabled)
          if (isMassTransferEnabled)
            OneDimensionalMiningConstraint(0, TxEstimators.one, "MaxTxsInKeyBlock")
          else
            minerSettings
              .map(ms => OneDimensionalMiningConstraint(ms.maxTransactionsInKeyBlock, TxEstimators.one, "MaxTxsInKeyBlock"))
              .getOrElse(MiningConstraint.Unlimited)
        else OneDimensionalMiningConstraint(ClassicAmountOfTxsInBlock, TxEstimators.one, "MaxTxsInKeyBlock"),
      micro =
        if (isNgEnabled && minerSettings.isDefined)
          OneDimensionalMiningConstraint(minerSettings.get.maxTransactionsInMicroBlock, TxEstimators.one, "MaxTxsInMicroBlock")
        else MiningConstraint.Unlimited
    )
  }
}
