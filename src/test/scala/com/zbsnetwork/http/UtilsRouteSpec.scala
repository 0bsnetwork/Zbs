package com.zbsnetwork.http

import com.zbsnetwork.crypto
import com.zbsnetwork.http.ApiMarshallers._
import com.zbsnetwork.lang.v1.compiler.Terms._
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.PureContext
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.state.diffs.CommonValidation
import com.zbsnetwork.utils.Time
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.{JsObject, JsValue}
import com.zbsnetwork.api.http.{TooBigArrayAllocation, UtilsApiRoute}
import com.zbsnetwork.common.utils.Base58
import com.zbsnetwork.lang.StdLibVersion
import com.zbsnetwork.transaction.smart.script.Script
import com.zbsnetwork.transaction.smart.script.v1.ExprScript

class UtilsRouteSpec extends RouteSpec("/utils") with RestAPISettingsHelper with PropertyChecks {
  private val route = UtilsApiRoute(
    new Time {
      def correctedTime(): Long = System.currentTimeMillis()
      def getTimestamp(): Long  = System.currentTimeMillis()
    },
    restAPISettings
  ).route

  val script = FUNCTION_CALL(
    function = PureContext.eq.header,
    args = List(CONST_LONG(1), CONST_LONG(2))
  )

  routePath("/script/decompile") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64
    Post(routePath("/script/decompile"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe "(1 == 2)"
    }
  }

  routePath("/script/compile") in {
    Post(routePath("/script/compile"), "(1 == 2)") ~> route ~> check {
      val json           = responseAs[JsValue]
      val expectedScript = ExprScript(StdLibVersion.V2, script).explicitGet()

      Script.fromBase64String((json \ "script").as[String]) shouldBe Right(expectedScript)
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  routePath("/script/estimate") in {
    val base64 = ExprScript(script).explicitGet().bytes().base64

    Post(routePath("/script/estimate"), base64) ~> route ~> check {
      val json = responseAs[JsValue]
      (json \ "script").as[String] shouldBe base64
      (json \ "scriptText").as[String] shouldBe "FUNCTION_CALL(Native(0),List(CONST_LONG(1), CONST_LONG(2)))" // [WAIT] s"(1 == 2)"
      (json \ "complexity").as[Long] shouldBe 3
      (json \ "extraFee").as[Long] shouldBe CommonValidation.ScriptExtraFee
    }
  }

  routePath("/seed") in {
    Get(routePath("/seed")) ~> route ~> check {
      val seed = Base58.decode((responseAs[JsValue] \ "seed").as[String])
      seed shouldBe 'success
      seed.get.length shouldEqual UtilsApiRoute.DefaultSeedSize
    }
  }

  routePath("/seed/{length}") in forAll(Gen.posNum[Int]) { l =>
    if (l > UtilsApiRoute.MaxSeedSize) {
      Get(routePath(s"/seed/$l")) ~> route should produce(TooBigArrayAllocation)
    } else {
      Get(routePath(s"/seed/$l")) ~> route ~> check {
        val seed = Base58.decode((responseAs[JsValue] \ "seed").as[String])
        seed shouldBe 'success
        seed.get.length shouldEqual l
      }
    }
  }

  for ((hash, f) <- Seq[(String, String => Array[Byte])](
         "secure" -> crypto.secureHash,
         "fast"   -> crypto.fastHash
       )) {
    val uri = routePath(s"/hash/$hash")
    uri in {
      forAll(Gen.alphaNumStr) { s =>
        Post(uri, s) ~> route ~> check {
          val r = responseAs[JsObject]
          (r \ "message").as[String] shouldEqual s
          (r \ "hash").as[String] shouldEqual Base58.encode(f(s))
        }
      }
    }
  }
}
