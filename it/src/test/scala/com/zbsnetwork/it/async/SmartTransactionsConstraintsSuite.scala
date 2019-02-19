package com.zbsnetwork.it.async

import com.typesafe.config.Config
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.api.http.assets.SignedSetScriptRequest
import com.zbsnetwork.common.utils.{Base58, EitherExt2}
import com.zbsnetwork.it.api.AsyncHttpApi._
import com.zbsnetwork.it.transactions.NodesFromDocker
import com.zbsnetwork.it.{NodeConfigs, TransferSending}
import com.zbsnetwork.lang.StdLibVersion.V1
import com.zbsnetwork.lang.v1.compiler.Terms
import com.zbsnetwork.mining.MiningConstraints.MaxScriptRunsInBlock
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import org.scalatest._
import play.api.libs.json.{JsNumber, Json}

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

class SmartTransactionsConstraintsSuite extends FreeSpec with Matchers with TransferSending with NodesFromDocker {

  override protected val nodeConfigs: Seq[Config] = NodeConfigs.newBuilder
    .overrideBase(
      _.raw(
        s"""akka.http.server {
         |  parsing.max-content-length = 3737439
         |  request-timeout = 60s
         |}
         |
         |zbs {
         |  network.enable-peers-exchange = no
         |
         |  miner {
         |    quorum = 0
         |    minimal-block-generation-offset = 60000ms
         |    micro-block-interval = 3s
         |    max-transactions-in-key-block = 0
         |    max-transactions-in-micro-block = 500
         |  }
         |
         |  blockchain.custom {
         |    functionality {
         |      feature-check-blocks-period = 1
         |      blocks-for-feature-activation = 1
         |
         |      pre-activated-features {
         |        2: 0
         |        4: 0
         |      }
         |    }
         |
         |    store-transactions-in-state = false
         |  }
         |
         |  features.supported = [2, 4]
         |}""".stripMargin
      ))
    .withDefault(1)
    .build(false)

  private def miner                   = nodes.head
  private val smartAccountPrivateKey  = PrivateKeyAccount.fromSeed(NodeConfigs.Default(1).getString("account-seed")).explicitGet()
  private val simpleAccountPrivateKey = PrivateKeyAccount.fromSeed(NodeConfigs.Default(2).getString("account-seed")).explicitGet()

  s"Block is limited by size after activation" in result(
    for {
      _ <- miner.signedBroadcast(Json.toJsObject(toRequest(setScriptTx(smartAccountPrivateKey))) + ("type" -> JsNumber(13)))
      _ <- processRequests(generateTransfersFromAccount(MaxScriptRunsInBlock * 3, smartAccountPrivateKey.address))
      _ <- miner.waitForHeight(5)
      _ <- processRequests(generateTransfersFromAccount(MaxScriptRunsInBlock * 3, smartAccountPrivateKey.address))
      _ <- scala.concurrent.Future.sequence((0 to 9).map(_ =>
        processRequests(generateTransfersFromAccount((50 - MaxScriptRunsInBlock / 10), simpleAccountPrivateKey.address))))
      _                  <- miner.waitForHeight(6)
      blockWithSetScript <- miner.blockHeadersAt(2)
      restBlocks         <- miner.blockHeadersSeq(3, 4)
      newBlock           <- miner.blockHeadersAt(5)
    } yield {
      blockWithSetScript.transactionCount should (be <= (MaxScriptRunsInBlock + 1) and be >= 1)
      restBlocks.foreach { x =>
        x.transactionCount should be(MaxScriptRunsInBlock)
      }
      newBlock.transactionCount should be > MaxScriptRunsInBlock
    },
    12.minutes
  )

  private def setScriptTx(sender: PrivateKeyAccount) =
    SetScriptTransaction
      .selfSigned(
        sender = sender,
        script = Some(ExprScript(V1, Terms.TRUE, checkSize = false).explicitGet()),
        fee = 1000000,
        timestamp = System.currentTimeMillis() - 5.minutes.toMillis
      )
      .explicitGet()

  private def toRequest(tx: SetScriptTransaction): SignedSetScriptRequest = SignedSetScriptRequest(
    senderPublicKey = Base58.encode(tx.sender.publicKey),
    script = tx.script.map(_.bytes().base64),
    fee = tx.fee,
    timestamp = tx.timestamp,
    proofs = tx.proofs.proofs.map(_.base58)(collection.breakOut)
  )

}
