package com.zbsplatform.it.sync.smartcontract

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.{ReportingTestName, WaitForHeight2}
import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.it.util._
import com.zbsplatform.it.sync._
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.transfer.{TransferTransactionV2}
import com.zbsplatform.utils.dummyCompilerContext
import org.scalatest.{CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.JsNumber

class UTXAllowance extends FreeSpec with Matchers with WaitForHeight2 with CancelAfterFailure with ReportingTestName with NodesFromDocker {
  import UTXAllowance._

  override protected def nodeConfigs: Seq[Config] = Configs

  private def nodeA = nodes.head
  private def nodeB = nodes.last

  "create two nodes with scripted accounts and check UTX" in {
    val accounts = List(nodeA, nodeB).map(i => {

      val nodeAddress = i.createAddress()
      val acc         = PrivateKeyAccount.fromSeed(i.seed(nodeAddress)).right.get

      val tx = i.transfer(i.address, nodeAddress, 10.zbs, 0.005.zbs).id
      nodes.waitForHeightAriseAndTxPresent(tx)

      val scriptText = {
        val sc = Parser(s"""true""".stripMargin).get.value
        CompilerV1(dummyCompilerContext, sc).explicitGet()._1
      }

      val script = ScriptV1(scriptText).explicitGet()
      val setScriptTransaction = SetScriptTransaction
        .selfSigned(SetScriptTransaction.supportedVersions.head, acc, Some(script), minFee, System.currentTimeMillis())
        .right
        .get

      val setScriptId = i
        .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
        .id

      nodes.waitForHeightAriseAndTxPresent(setScriptId)
      acc
    })

    val txA =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = accounts(0),
          recipient = accounts(0),
          amount = 1.zbs,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.004.zbs,
          attachment = Array.emptyByteArray
        )
        .right
        .get
    assertBadRequestAndMessage(
      nodeA.signedBroadcast(txA.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id,
      "transactions from scripted accounts are denied from UTX pool"
    )

    val txB =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = accounts(1),
          recipient = accounts(1),
          amount = 1.zbs,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = minFee + 0.004.zbs,
          attachment = Array.emptyByteArray
        )
        .right
        .get

    val txBId = nodeB.signedBroadcast(txB.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id
    nodes.waitForHeightArise()
    nodeA.findTransactionInfo(txBId) shouldBe None
  }

}

object UTXAllowance {
  import com.zbsplatform.it.NodeConfigs._
  private val FirstNode = ConfigFactory.parseString(s"""
                                                         |zbs {
                                                         |  utx.allow-transactions-from-smart-accounts = false
                                                         |  miner {
                                                         |      quorum = 0
                                                         |      enable = yes
                                                         |  }
                                                         |}""".stripMargin)

  private val SecondNode = ConfigFactory.parseString(s"""
                                                          |zbs {
                                                          |  utx.allow-transactions-from-smart-accounts = true
                                                          |  miner {
                                                          |      enable = no
                                                          |  }
                                                          |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    FirstNode.withFallback(Default.head),
    SecondNode.withFallback(Default(1))
  )

}
