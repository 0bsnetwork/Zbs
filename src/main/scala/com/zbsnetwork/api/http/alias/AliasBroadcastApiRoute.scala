package com.zbsnetwork.api.http.alias

import akka.http.scaladsl.server.Route
import com.zbsnetwork.api.http._
import com.zbsnetwork.http.BroadcastRoute
import com.zbsnetwork.settings.RestAPISettings
import com.zbsnetwork.utx.UtxPool
import io.netty.channel.group.ChannelGroup

case class AliasBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      doBroadcast(aliasReq.toTx)
    }
  }
}
