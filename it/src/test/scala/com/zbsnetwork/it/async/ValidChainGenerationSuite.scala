package com.zbsnetwork.it.async

import com.typesafe.config.Config
import com.zbsnetwork.it.api.AsyncHttpApi._
import com.zbsnetwork.it.transactions.NodesFromDocker
import com.zbsnetwork.it.{NodeConfigs, TransferSending, WaitForHeight2}
import org.scalatest._

import scala.concurrent.Await.result
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future.traverse
import scala.concurrent.duration._
import scala.util.Random

class ValidChainGenerationSuite extends FreeSpec with WaitForHeight2 with TransferSending with NodesFromDocker with CancelAfterFailure {

  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(2))
      .withDefault(2)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  "Generate more blocks and resynchronise after rollback" - {
    "1 of N" in test(1)
    "N-1 of N" in test(nodes.size - 1)

    def test(n: Int): Unit =
      result(
        for {
          height <- traverse(nodes)(_.height).map(_.max)
          baseHeight = height + 5
          _ <- traverse(nodes)(_.waitForHeight(baseHeight))

          rollbackNodes = Random.shuffle(nodes).take(n)
          _ <- traverse(rollbackNodes)(_.rollback(1))
          _ <- nodes.waitForSameBlockHeadesAt(baseHeight)
        } yield (),
        7.minutes
      )
  }
}
