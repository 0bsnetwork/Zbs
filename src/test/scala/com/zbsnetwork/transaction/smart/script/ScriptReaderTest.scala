package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.crypto
import com.zbsnetwork.lang.Version.{ExprV1, ContractV}
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.contract.Contract.{CallableAnnotation, ContractFunction}
import com.zbsnetwork.lang.v1.Serde
import com.zbsnetwork.lang.v1.compiler.Terms
import com.zbsnetwork.lang.v1.compiler.Terms.TRUE
import com.zbsnetwork.state.diffs.produce
import com.zbsnetwork.transaction.smart.script.v1.ContractScript
import org.scalatest.{FreeSpec, Matchers}

class ScriptReaderTest extends FreeSpec with Matchers {
  val checksumLength = 4

  "should parse all bytes for V1" in {
    val body     = Array(ExprV1.toByte) ++ Serde.serialize(TRUE) ++ "foo".getBytes
    val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
    ScriptReader.fromBytes(allBytes) should produce("bytes left")
  }

  "should parse all bytes for V3" in {
    val sc =
      ContractScript(ContractV, Contract(List.empty, List(ContractFunction(CallableAnnotation("sender"), Terms.FUNC("foo", List("a"), Terms.REF("a")))), None))

    val allBytes = sc.bytes().arr
    ScriptReader.fromBytes(allBytes) shouldBe Right(sc)
  }

}
