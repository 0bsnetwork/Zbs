package com.zbsnetwork.it.sync.network

import java.nio.charset.StandardCharsets

import com.typesafe.config.Config
import com.zbsnetwork.account.Address
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.AsyncNetworkApi._
import com.zbsnetwork.it.transactions.BaseTransactionSuite
import com.zbsnetwork.network.{RawBytes, TransactionSpec}
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.it.NodeConfigs
import com.zbsnetwork.transaction.transfer._
import org.scalatest._
import com.zbsnetwork.it.sync._

import scala.concurrent.duration._
import scala.language.postfixOps

class SimpleTransactionsSuite extends BaseTransactionSuite with Matchers {
  override protected def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .withDefault(entitiesNumber = 1)
      .buildNonConflicting()

  private def node = nodes.head

  test("valid tx send by network to node should be in blockchain") {
    val tx = TransferTransactionV1
      .selfSigned(None, node.privateKey, Address.fromString(node.address).explicitGet(), 1L, System.currentTimeMillis(), None, minFee, Array())
      .right
      .get

    node.sendByNetwork(RawBytes.from(tx))
    node.waitForTransaction(tx.id().base58)

  }

  test("invalid tx send by network to node should be not in UTX or blockchain") {
    val tx = TransferTransactionV1
      .selfSigned(None,
                  node.privateKey,
                  Address.fromString(node.address).explicitGet(),
                  1L,
                  System.currentTimeMillis() + (1 days).toMillis,
                  None,
                  minFee,
                  Array())
      .right
      .get

    node.sendByNetwork(RawBytes.from(tx))
    val maxHeight = nodes.map(_.height).max
    nodes.waitForHeight(maxHeight + 1)
    node.ensureTxDoesntExist(tx.id().base58)
  }

  test("should blacklist senders of non-parsable transactions") {
    val blacklistBefore = node.blacklistedPeers
    node.sendByNetwork(RawBytes(TransactionSpec.messageCode, "foobar".getBytes(StandardCharsets.UTF_8)))
    node.waitForBlackList(blacklistBefore.size)
  }
}
