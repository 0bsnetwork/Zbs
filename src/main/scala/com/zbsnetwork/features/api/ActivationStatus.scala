package com.zbsplatform.features.api

case class ActivationStatus(height: Int, votingInterval: Int, votingThreshold: Int, nextCheck: Int, features: Seq[FeatureActivationStatus])
