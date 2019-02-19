package com.zbsnetwork.features.api

import com.zbsnetwork.features.BlockchainFeatureStatus

case class FeatureActivationStatus(id: Short,
                                   description: String,
                                   blockchainStatus: BlockchainFeatureStatus,
                                   nodeStatus: NodeFeatureStatus,
                                   activationHeight: Option[Int],
                                   supportingBlocks: Option[Int])
