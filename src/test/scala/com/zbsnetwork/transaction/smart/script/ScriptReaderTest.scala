package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.crypto
import com.zbsnetwork.lang.StdLibVersion.{V3, V1}
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.contract.Contract.{CallableAnnotation, CallableFunction}
import com.zbsnetwork.lang.v1.Serde
import com.zbsnetwork.lang.v1.compiler.Terms
import com.zbsnetwork.lang.v1.compiler.Terms.TRUE
import com.zbsnetwork.state.diffs.produce
import org.scalatest.{FreeSpec, Matchers}

class ScriptReaderTest extends FreeSpec with Matchers {
  val checksumLength = 4

  "should parse all bytes for V1" in {
    val body     = Array(V1.toByte) ++ Serde.serialize(TRUE) ++ "foo".getBytes
    val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
    ScriptReader.fromBytes(allBytes) should produce("bytes left")
  }

  "should parse all bytes for V3" in {
    val sc = ContractScript(
      V3,
      Contract(List.empty, List(CallableFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), Terms.REF("a")))), None)
    ).explicitGet()

    val allBytes = sc.bytes().arr
    ScriptReader.fromBytes(allBytes) shouldBe Right(sc)
  }

}
