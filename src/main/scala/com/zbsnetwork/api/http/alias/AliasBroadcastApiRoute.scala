package com.zbsplatform.api.http.alias

import akka.http.scaladsl.server.Route
import com.zbsplatform.settings.RestAPISettings
import com.zbsplatform.utx.UtxPool
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import com.zbsplatform.api.http._
import com.zbsplatform.http.BroadcastRoute

@Path("/alias/broadcast")
@Api(value = "/alias")
case class AliasBroadcastApiRoute(settings: RestAPISettings, utx: UtxPool, allChannels: ChannelGroup) extends ApiRoute with BroadcastRoute {
  override val route = pathPrefix("alias" / "broadcast") {
    signedCreate
  }

  @Path("/create")
  @ApiOperation(value = "Broadcasts a signed alias transaction", httpMethod = "POST", produces = "application/json", consumes = "application/json")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(
        name = "body",
        value = "Json with data",
        required = true,
        paramType = "body",
        dataType = "com.zbsplatform.api.http.alias.SignedCreateAliasV1Request",
        defaultValue =
          "{\n\t\"alias\": \"aliasalias\",\n\t\"senderPublicKey\": \"11111\",\n\t\"fee\": 100000\n\t\"timestamp\": 12345678,\n\t\"signature\": \"asdasdasd\"\n}"
      )
    ))
  @ApiResponses(Array(new ApiResponse(code = 200, message = "Json with response or error")))
  def signedCreate: Route = (path("create") & post) {
    json[SignedCreateAliasV1Request] { aliasReq =>
      doBroadcast(aliasReq.toTx)
    }
  }
}
