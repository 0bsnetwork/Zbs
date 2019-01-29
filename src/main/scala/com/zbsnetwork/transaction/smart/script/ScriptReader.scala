package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.crypto
import com.zbsnetwork.lang.Version
import com.zbsnetwork.lang.Version._
import com.zbsnetwork.lang.contract.ContractSerDe
import com.zbsnetwork.lang.v1.Serde
import com.zbsnetwork.transaction.ValidationError.ScriptParseError
import com.zbsnetwork.transaction.smart.script.v1._

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    (for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      ver = Version(version.toInt)
      sv <- Either
        .cond(
          SupportedVersions(ver),
          ver,
          ScriptParseError(s"Invalid version: $version")
        )
      s <- sv match {
        case ExprV1 | ExprV2 =>
          for {
            _     <- ExprScript.validateBytes(scriptBytes)
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(sv, bytes, checkSize = false)
          } yield s
        case ContractV =>
          for {
            bytes <- ContractSerDe.deserialize(scriptBytes)
            s = ContractScript(sv, bytes)
          } yield s
      }
    } yield s).left
      .map(m => ScriptParseError(m.toString))
  }

}
