package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.common.utils._
import com.zbsnetwork.lang.StdLibVersion._
import com.zbsnetwork.lang.v1.Serde
import com.zbsnetwork.lang.v1.testing.TypedScriptGen
import com.zbsnetwork.state.diffs.produce
import com.zbsnetwork.{NoShrink, crypto}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Inside, Matchers, PropSpec}

class ScriptReaderTest extends PropSpec with PropertyChecks with Matchers with TypedScriptGen with Inside with NoShrink {
  val checksumLength = 4

  property("should parse all bytes for V1") {
    forAll(exprGen) { sc =>
      val body     = Array(V1.toByte) ++ Serde.serialize(sc) ++ "foo".getBytes
      val allBytes = body ++ crypto.secureHash(body).take(checksumLength)
      ScriptReader.fromBytes(allBytes) should produce("bytes left")
    }
  }

  property("should parse all bytes for V3") {
    forAll(contractGen) { sc =>
      val allBytes = ContractScript.apply(V3, sc).explicitGet().bytes().arr
      ScriptReader.fromBytes(allBytes).explicitGet().expr shouldBe sc
    }
  }
}
