package com.zbsplatform.http

import com.zbsplatform.TestWallet
import com.zbsplatform.settings.ZbsSettings
import com.zbsplatform.api.http.ApiKeyNotValid

class DebugApiRouteSpec extends RouteSpec("/debug") with RestAPISettingsHelper with TestWallet {
  private val sampleConfig = com.typesafe.config.ConfigFactory.load()
  private val zbsSettings  = ZbsSettings.fromConfig(sampleConfig)
  private val configObject = sampleConfig.root()
  private val route =
    DebugApiRoute(zbsSettings, null, null, null, null, null, null, null, null, null, null, null, null, null, configObject).route

  routePath("/configInfo") - {
    "requires api-key header" in {
      Get(routePath("/configInfo?full=true")) ~> route should produce(ApiKeyNotValid)
      Get(routePath("/configInfo?full=false")) ~> route should produce(ApiKeyNotValid)
    }
  }
}
