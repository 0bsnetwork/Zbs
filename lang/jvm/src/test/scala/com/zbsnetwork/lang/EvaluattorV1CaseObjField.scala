package com.zbsplatform.lang

import cats.kernel.Monoid
import com.zbsplatform.lang.Common._
import com.zbsplatform.lang.v1.compiler.Terms._
import com.zbsplatform.lang.v1.evaluator.ctx.EvaluationContext._
import com.zbsplatform.lang.v1.evaluator.ctx._
import com.zbsplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.zbsplatform.lang.v1.evaluator.ctx.impl.PureContext._
import com.zbsplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class EvaluattorV1CaseObjField extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {

  def context(p: CaseObj): EvaluationContext = Monoid.combine(PureContext.evalContext, sampleUnionContext(p))

  property("case custom type field access") {
    ev[Long](
      context = context(pointAInstance),
      expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), "X"), CONST_LONG(2L)))
    ) shouldBe Right(5)
  }

  property("case custom type field access over union") {
    def testAccess(instance: CaseObj, field: String) =
      ev[Long](
        context = context(instance),
        expr = FUNCTION_CALL(sumLong.header, List(GETTER(REF("p"), field), CONST_LONG(2L)))
      )

    testAccess(pointAInstance, "X") shouldBe Right(5)
    testAccess(pointBInstance, "X") shouldBe Right(5)
    testAccess(pointAInstance, "YA") shouldBe Right(42)
    testAccess(pointBInstance, "YB") shouldBe Right(43)
  }
}
