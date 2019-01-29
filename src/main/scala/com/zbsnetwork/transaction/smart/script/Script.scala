package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.Base64
import com.zbsnetwork.lang.Version._
import com.zbsnetwork.transaction.ValidationError.ScriptParseError
import monix.eval.Coeval

trait Script {
  type Expr

  val version: Version

  val expr: Expr
  val text: String
  val bytes: Coeval[ByteStr]
  val complexity: Long

  val containsBlockV2: Coeval[Boolean]

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: Script => version == that.version && expr == that.expr
    case _            => false
  }

  override def hashCode(): Int = version * 31 + expr.hashCode()
}

object Script {

  val checksumLength = 4

  def fromBase64String(str: String): Either[ScriptParseError, Script] =
    for {
      bytes  <- Base64.decode(str).toEither.left.map(ex => ScriptParseError(s"Unable to decode base64: ${ex.getMessage}"))
      script <- ScriptReader.fromBytes(bytes)
    } yield script

}
