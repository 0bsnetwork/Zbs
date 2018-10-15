package com.zbsplatform.http

import java.time.Instant

import akka.http.scaladsl.server.Route
import com.zbsplatform.Shutdownable
import com.zbsplatform.settings.{Constants, RestAPISettings}
import com.zbsplatform.state.Blockchain
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.Json
import com.zbsplatform.api.http.{ApiRoute, CommonApiFunctions}
import com.zbsplatform.utils.ScorexLogging

@Path("/node")
@Api(value = "node")
case class NodeApiRoute(settings: RestAPISettings, blockchain: Blockchain, application: Shutdownable)
    extends ApiRoute
    with CommonApiFunctions
    with ScorexLogging {

  override lazy val route = pathPrefix("node") {
    stop ~ status ~ version
  }

  @Path("/version")
  @ApiOperation(value = "Version", notes = "Get Zbs node version", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json Zbs node version")
    ))
  def version: Route = (get & path("version")) {
    complete(Json.obj("version" -> Constants.AgentName))
  }

  @Path("/stop")
  @ApiOperation(value = "Stop", notes = "Stop the node", httpMethod = "POST")
  def stop: Route = (post & path("stop") & withAuth) {
    log.info("Request to stop application")
    application.shutdown()
    complete(Json.obj("stopped" -> true))
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get status of the running core", httpMethod = "GET")
  def status: Route = (get & path("status")) {
    val lastUpdated = blockchain.lastBlock.get.timestamp
    complete(
      Json.obj(
        "blockchainHeight" -> blockchain.height,
        "stateHeight"      -> blockchain.height,
        "updatedTimestamp" -> lastUpdated,
        "updatedDate"      -> Instant.ofEpochMilli(lastUpdated).toString
      ))
  }
}
