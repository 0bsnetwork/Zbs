package com.zbsnetwork.it.matcher

import com.typesafe.config.Config
import com.zbsnetwork.it._
import com.zbsnetwork.it.transactions.NodesFromDocker
import org.scalatest._
import com.zbsnetwork.it.util._
import scala.concurrent.ExecutionContext

abstract class MatcherSuiteBase
    extends FreeSpec
    with Matchers
    with CancelAfterFailure
    with ReportingTestName
    with NodesFromDocker
    with BeforeAndAfterAll
    with MatcherNode {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  val defaultAssetQuantity = 999999999999L

  val smartFee         = 0.004.zbs
  val minFee           = 0.02.zbs + smartFee
  val issueFee         = 500.zbs
  val smartIssueFee    = 1.zbs + smartFee
  val leasingFee       = 5.zbs + smartFee
  val tradeFee         = 0.02.zbs
  val smartTradeFee    = tradeFee + smartFee
  val twoSmartTradeFee = tradeFee + 2 * smartFee

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .withDefault(4)
      .buildNonConflicting()

}
