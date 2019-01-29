package com.zbsnetwork.it.sync

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.it.Node
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.transactions.NodesFromDocker
import com.zbsnetwork.transaction.transfer.TransferTransactionV1
import monix.eval.Task
import monix.execution.Scheduler
import org.scalatest.{CancelAfterFailure, FunSuite, Matchers}

import scala.concurrent.duration._
import scala.util.{Random, Try}

class UtxSuite extends FunSuite with CancelAfterFailure with NodesFromDocker with Matchers {
  override protected def nodeConfigs: Seq[Config] = UtxSuite.Configs

  val miner: Node    = nodes.head
  val notMiner: Node = nodes(1)

  val ENOUGH_FEE = 5000000
  val AMOUNT     = ENOUGH_FEE * 10

  test("Invalid transaction should be removed from from utx") {
    val seed = Array.fill(32)(-1: Byte)

    Random.nextBytes(seed)

    val account = PrivateKeyAccount(seed)

    val transferToAccount = TransferTransactionV1
      .selfSigned(None, miner.privateKey, account, AMOUNT, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    miner.signedBroadcast(transferToAccount.json())

    nodes.waitForHeightAriseAndTxPresent(transferToAccount.id().base58)

    val firstTransfer = TransferTransactionV1
      .selfSigned(None, account, miner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    val secondTransfer = TransferTransactionV1
      .selfSigned(None, account, notMiner.privateKey, AMOUNT - ENOUGH_FEE, System.currentTimeMillis(), None, ENOUGH_FEE, Array.emptyByteArray)
      .explicitGet()

    val tx1Id = miner.signedBroadcast(firstTransfer.json()).id
    val tx2Id = notMiner.signedBroadcast(secondTransfer.json()).id

    waitForEmptyUtx(nodes)

    val exactlyOneTxInBlockchain =
      txInBlockchain(tx1Id, nodes) ^ txInBlockchain(tx2Id, nodes)

    assert(exactlyOneTxInBlockchain, "Only one tx should be in blockchain")
  }

  def waitForEmptyUtx(nodes: Seq[Node]): Unit = {
    implicit val sch: Scheduler = monix.execution.Scheduler.global

    def loop(): Task[Unit] = {
      val utxIds = nodes.map(_.utx.size)

      if (utxIds.sum != 0) {
        Task
          .sleep(1.second)
          .flatMap(_ => loop())
      } else {
        Task.pure(())
      }
    }

    loop().runSyncUnsafe(6.minutes)
  }

  def txInBlockchain(txId: String, nodes: Seq[Node]): Boolean = {
    nodes.forall { node =>
      Try(node.transactionInfo(txId)).isSuccess
    }
  }
}

object UtxSuite {
  import com.zbsnetwork.it.NodeConfigs._
  private val minerConfig = ConfigFactory.parseString(s"""
                                                         |zbs {
                                                         |  synchronization.synchronization-timeout = 10s
                                                         |  blockchain.custom.functionality {
                                                         |    pre-activated-features.1 = 0
                                                         |    generation-balance-depth-from-50-to-1000-after-height = 100
                                                         |  }
                                                         |  miner.quorum = 0
                                                         |}""".stripMargin)

  private val notMinerConfig = ConfigFactory.parseString(s"""
                                                            |zbs {
                                                            |  synchronization.synchronization-timeout = 10s
                                                            |  blockchain.custom.functionality {
                                                            |    pre-activated-features.1 = 0
                                                            |    generation-balance-depth-from-50-to-1000-after-height = 100
                                                            |  }
                                                            |  miner.enable = no
                                                            |}""".stripMargin)

  val Configs: Seq[Config] = Seq(
    minerConfig.withFallback(Default.head),
    notMinerConfig.withFallback(Default(1)),
  )

}
