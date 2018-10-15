package com.zbsplatform.http

import akka.http.scaladsl.server.Route
import com.zbsplatform.BlockGen
import com.zbsplatform.db.WithState
import com.zbsplatform.http.ApiMarshallers._
import com.zbsplatform.settings.FunctionalitySettings
import com.zbsplatform.state._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.JsObject
import com.zbsplatform.api.http.BlockDoesNotExist
import com.zbsplatform.consensus.nxt.api.http.NxtConsensusApiRoute

class ConsensusRouteSpec
    extends RouteSpec("/consensus")
    with RestAPISettingsHelper
    with PropertyChecks
    with BlockGen
    with HistoryTest
    with WithState {

  def routeTest(f: (Blockchain, Route) => Any) = withDomain() { d =>
    d.blockchainUpdater.processBlock(genesisBlock)
    1 to 10 foreach { _ =>
      d.blockchainUpdater.processBlock(getNextTestBlock(d.blockchainUpdater))
    }
    f(d.blockchainUpdater, NxtConsensusApiRoute(restAPISettings, d.blockchainUpdater, FunctionalitySettings.TESTNET).route)
  }

  routePath("/generationsignature") - {
    "for last block" in routeTest { (h, route) =>
      Get(routePath("/generationsignature")) ~> route ~> check {
        (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual h.lastBlock.get.consensusData.generationSignature.base58
      }
    }

    "for existing block" in routeTest { (h, route) =>
      val block = h.blockAt(3).get
      Get(routePath(s"/generationsignature/${block.uniqueId.base58}")) ~> route ~> check {
        (responseAs[JsObject] \ "generationSignature").as[String] shouldEqual block.consensusData.generationSignature.base58
      }
    }

    "for non-existent block" in routeTest { (h, route) =>
      Get(routePath(s"/generationsignature/brggwg4wg4g")) ~> route should produce(BlockDoesNotExist)
    }
  }

  routePath("/basetarget") - {
    "for existing block" in routeTest { (h, route) =>
      val block = h.blockAt(3).get
      Get(routePath(s"/basetarget/${block.uniqueId.base58}")) ~> route ~> check {
        (responseAs[JsObject] \ "baseTarget").as[Long] shouldEqual block.consensusData.baseTarget
      }
    }

    "for non-existent block" in routeTest { (h, route) =>
      Get(routePath(s"/basetarget/brggwg4wg4g")) ~> route should produce(BlockDoesNotExist)
    }
  }
}
