package com.zbsplatform.features.api

import com.zbsplatform.features.BlockchainFeatureStatus

case class FeatureActivationStatus(id: Short,
                                   description: String,
                                   blockchainStatus: BlockchainFeatureStatus,
                                   nodeStatus: NodeFeatureStatus,
                                   activationHeight: Option[Int],
                                   supportingBlocks: Option[Int])
