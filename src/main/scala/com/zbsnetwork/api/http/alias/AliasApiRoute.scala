package com.zbsnetwork.api.http.alias

import akka.http.scaladsl.server.Route
import com.zbsnetwork.account.Alias
import com.zbsnetwork.api.http._
import com.zbsnetwork.http.BroadcastRoute
import com.zbsnetwork.settings.RestAPISettings
import com.zbsnetwork.state.Blockchain
import com.zbsnetwork.transaction._
import com.zbsnetwork.utils.Time
import com.zbsnetwork.utx.UtxPool
import com.zbsnetwork.wallet.Wallet
import io.netty.channel.group.ChannelGroup
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, Json}

@Path("/alias")
@Api(value = "/alias")
case class AliasApiRoute(settings: RestAPISettings, wallet: Wallet, utx: UtxPool, allChannels: ChannelGroup, time: Time, blockchain: Blockchain)
    extends ApiRoute
    with BroadcastRoute {

  override val route = pathPrefix("alias") {
    alias ~ addressOfAlias ~ aliasOfAddress
  }

  def alias: Route = processRequest("create", (t: CreateAliasV1Request) => doBroadcast(TransactionFactory.aliasV1(t, wallet, time)))

  @Path("/by-alias/{alias}")
  @ApiOperation(
    value = "Address by alias",
    notes = "Returns an address associated with an Alias. Alias should be plain text without an 'alias' prefix and network code.",
    httpMethod = "GET"
  )
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "alias", value = "Alias", required = true, dataType = "string", paramType = "path")
    ))
  def addressOfAlias: Route = (get & path("by-alias" / Segment)) { aliasName =>
    val result = Alias.buildWithCurrentChainId(aliasName) match {
      case Right(alias) =>
        blockchain.resolveAlias(alias) match {
          case Right(addr) => Right(Address(addr.stringRepr))
          case _           => Left(AliasDoesNotExist(alias))
        }
      case Left(err) => Left(ApiError.fromValidationError(err))
    }
    complete(result)
  }

  @Path("/by-address/{address}")
  @ApiOperation(value = "Aliases by address", notes = "Returns a collection of aliases associated with an address", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "address", value = "Address", required = true, dataType = "string", paramType = "path")
    ))
  def aliasOfAddress: Route = (get & path("by-address" / Segment)) { addressString =>
    val result: Either[ApiError, Seq[String]] = com.zbsnetwork.account.Address
      .fromString(addressString)
      .map(acc => blockchain.aliasesOfAddress(acc).map(_.stringRepr))
      .left
      .map(ApiError.fromValidationError)
    complete(result)
  }

  case class Address(address: String)

  implicit val addressFormat: Format[Address] = Json.format
}
