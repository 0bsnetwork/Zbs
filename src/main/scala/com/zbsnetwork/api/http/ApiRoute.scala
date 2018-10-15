package com.zbsplatform.api.http

import java.util.NoSuchElementException

import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.server._
import com.zbsplatform.crypto
import com.zbsplatform.http.{ApiMarshallers, PlayJsonException, api_key, deprecated_api_key}
import com.zbsplatform.settings.RestAPISettings
import com.zbsplatform.utils.Base58
import play.api.libs.json.{JsResultException, Reads}

trait ApiRoute extends Directives with CommonApiFunctions with ApiMarshallers {
  val settings: RestAPISettings
  val route: Route

  private lazy val apiKeyHash = Base58.decode(settings.apiKeyHash).toOption

  private val jsonRejectionHandler = RejectionHandler
    .newBuilder()
    .handle {
      case ValidationRejection(_, Some(PlayJsonException(cause, errors))) => complete(WrongJson(cause, errors))
    }
    .result()

  def json[A: Reads](f: A => ToResponseMarshallable): Route = handleRejections(jsonRejectionHandler) {
    entity(as[A]) { a =>
      complete(f(a))
    }
  }

  val jsonExceptionHandler = ExceptionHandler {
    case JsResultException(err)    => complete(WrongJson(errors = err))
    case e: NoSuchElementException => complete(WrongJson(Some(e)))
  }

  def withAuth: Directive0 = apiKeyHash.fold[Directive0](complete(ApiKeyNotValid)) { hashFromSettings =>
    optionalHeaderValueByType[api_key](()).flatMap {
      case Some(k) if crypto.secureHash(k.value.getBytes()).sameElements(hashFromSettings) => pass
      case _ =>
        optionalHeaderValueByType[deprecated_api_key](()).flatMap {
          case Some(k) if crypto.secureHash(k.value.getBytes()).sameElements(hashFromSettings) => pass
          case _                                                                               => complete(ApiKeyNotValid)
        }
    }
  }

  def processRequest[A: Reads](pathMatcher: String, f: A => ToResponseMarshallable): Route =
    (path(pathMatcher) & post & withAuth) {
      json[A](f)
    }
}
