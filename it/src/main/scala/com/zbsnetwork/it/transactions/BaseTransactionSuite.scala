package com.zbsplatform.it.transactions

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsplatform.it._
import monix.eval.Coeval
import org.scalatest.{BeforeAndAfterAll, FunSuite}

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext

abstract class BaseTransactionSuite
    extends FunSuite
    with WaitForHeight2
    with IntegrationSuiteWithThreeAddresses
    with BeforeAndAfterAll
    with NodesFromDocker {

  protected implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(3))
      .withDefault(3)
      .withSpecial(_.nonMiner)
      .buildNonConflicting()

  override def notMiner: Node = nodes.last

  // protected because https://github.com/sbt/zinc/issues/292
  protected val theNodes: Coeval[Seq[Node]] = Coeval.evalOnce {
    Option(System.getProperty("zbs.it.config.file")) match {
      case None => dockerNodes()
      case Some(filePath) =>
        val defaultConfig = ConfigFactory.load()
        ConfigFactory
          .parseFile(new File(filePath))
          .getConfigList("nodes")
          .asScala
          .map(cfg => new ExternalNode(cfg.withFallback(defaultConfig).resolve()))
    }
  }

  override protected def nodes: Seq[Node] = theNodes()

  protected override def beforeAll(): Unit = {
    theNodes.run
    super.beforeAll()
  }
}
