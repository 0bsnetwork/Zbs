package com.zbsnetwork.lang

import com.zbsnetwork.lang.directives.DirectiveKey._
import com.zbsnetwork.lang.directives.{Directive, DirectiveParser}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class DirectiveParserTest extends PropSpec with PropertyChecks with Matchers {

  def parse(s: String): List[Directive] = DirectiveParser(s)

  property("parse directives") {
    parse("{-# STDLIB_VERSION 10 #-}") shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
        |
        |{-# STDLIB_VERSION 10 #-}
        |
      """.stripMargin) shouldBe List(Directive(STDLIB_VERSION, "10"))
    parse("""
            |
            |{-# CONTENT_TYPE FOO #-}
            |
      """.stripMargin) shouldBe List(Directive(CONTENT_TYPE, "FOO"))
    parse("""
            |
            |{-# SCRIPT_TYPE BAR #-}
            |
      """.stripMargin) shouldBe List(Directive(SCRIPT_TYPE, "BAR"))
  }
}
