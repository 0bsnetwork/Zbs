package com.zbsnetwork.it.sync.matcher

import com.typesafe.config.{Config, ConfigFactory}
import com.zbsnetwork.account.PrivateKeyAccount
import com.zbsnetwork.it.Node
import com.zbsnetwork.it.NodeConfigs.Default
import com.zbsnetwork.it.api.SyncHttpApi._
import com.zbsnetwork.it.api.SyncMatcherHttpApi._
import com.zbsnetwork.it.matcher.{MatcherCommand, MatcherState, MatcherSuiteBase}
import com.zbsnetwork.it.sync.matcher.config.MatcherDefaultConfig._
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, Order}
import org.scalacheck.Gen

import scala.concurrent.duration.DurationInt
import scala.util.Random

// Works only with kafka
class MultipleMatchersTestSuite extends MatcherSuiteBase {
  private def configOverrides = ConfigFactory.parseString("""zbs.matcher {
      |  price-assets = ["ZBS"]
      |  snapshots-interval = 51
      |}""".stripMargin)

  private def matcher2NodeConfig = ConfigFactory.parseString("""zbs.network.node-name = node11
      |akka.kafka.consumer.kafka-clients.group.id = 1""".stripMargin).withFallback(Default.last)

  override protected def nodeConfigs: Seq[Config] =
    List(Default.last, matcher2NodeConfig, Default(2 + Random.nextInt(Default.size - 2)))
      .zip(Seq(matcherConfig, matcherConfig, minerEnabled))
      .map { case (n, o) => o.withFallback(n) }
      .map(configOverrides.withFallback)

  private def matcherPublicKey = matcher1Node.publicKey
  private def matcher1Node     = nodes.head
  private def matcher2Node     = nodes(1)

  private val placesNumber  = 200
  private val cancelsNumber = placesNumber / 10

  private val (issue1, issue2, assetPair1) = issueAssetPair(aliceAcc, 8, 8)
  private val assetPair2                   = AssetPair(assetPair1.amountAsset, None)
  private val assetPair3                   = AssetPair(assetPair1.priceAsset, None)
  private val assetPairs                   = Seq(assetPair1, assetPair2, assetPair3)

  // Issue assets by Alice
  private val assetIds = Seq(issue1, issue2)
    .map(x => x -> matcher1Node.signedIssue(x))
    .map { case (req, tx) => nodes.waitForTransaction(tx.id).id -> req }
    .toMap

  // Share assets with Bob
  assetIds
    .map {
      case (id, info) =>
        aliceNode.transfer(aliceAcc.toAddress.stringRepr, bobNode.publicKey.toAddress.stringRepr, info.quantity / 2, minFee, Some(id)).id
    }
    .foreach(nodes.waitForTransaction)

  private val aliceOrders = mkOrders(aliceAcc)
  private val bobOrders   = mkOrders(aliceAcc)
  private val orders      = aliceOrders ++ bobOrders

  "Place, fill and cancel a lot of orders" in {
    val alicePlaces = aliceOrders.map(MatcherCommand.Place(matcher1Node, _))
    val bobPlaces   = bobOrders.map(MatcherCommand.Place(matcher2Node, _))
    val places      = Random.shuffle(alicePlaces ++ bobPlaces)

    val aliceCancels = (1 to cancelsNumber).map(_ => choose(aliceOrders)).map(MatcherCommand.Cancel(matcher1Node, aliceAcc, _))
    val bobCancels   = (1 to cancelsNumber).map(_ => choose(bobOrders)).map(MatcherCommand.Cancel(matcher2Node, bobAcc, _))
    val cancels      = Random.shuffle(aliceCancels ++ bobCancels)

    executeCommands(places ++ cancels)
  }

  "Wait until all requests are processed" in {
    matcher1Node.waitForStableOffset(10, 100, 200.millis)
    matcher2Node.waitForStableOffset(10, 100, 200.millis)
  }

  "States on both matcher should be equal" in {
    val state1 = state(matcher1Node)
    val state2 = state(matcher2Node)
    state1 shouldBe state2
  }

  private def mkOrders(account: PrivateKeyAccount) =
    Gen.containerOfN[Vector, Order](placesNumber, orderGen(matcherPublicKey, account, assetPairs)).sample.get

  private def state(matcherNode: Node) = clean(matcherNode.matcherState(assetPairs, orders, Seq(aliceAcc, bobAcc)))

  // Because we can't guarantee that SaveSnapshot message will come at same place in a orderbook's queue on both matchers
  private def clean(state: MatcherState): MatcherState = state.copy(
    snapshots = state.snapshots.map { case (k, _) => k -> 0L }
  )
}