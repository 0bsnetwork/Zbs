package com.zbsnetwork.transaction.smart.script

import com.zbsnetwork.crypto
import com.zbsnetwork.lang.contract.ContractSerDe
import com.zbsnetwork.lang.v1.Serde
import com.zbsnetwork.lang.{ContentType, StdLibVersion}
import com.zbsnetwork.transaction.ValidationError.ScriptParseError
import com.zbsnetwork.transaction.smart.script.v1._

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum          = bytes.takeRight(checksumLength)
    val computedCheckSum  = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val versionByte: Byte = bytes.head
    (for {
      a <- {
        if (versionByte == 0)
          Right((ContentType.parseId(bytes(1)), StdLibVersion.parseVersion(bytes(2)), 3))
        else if (versionByte == StdLibVersion.V1.toByte || versionByte == StdLibVersion.V2.toByte)
          Right((ContentType.Expression, StdLibVersion(versionByte.toInt), 1))
        else Left(ScriptParseError(s"Can't parse script bytes starting with [${bytes(0).toInt},${bytes(1).toInt},${bytes(2).toInt}]"))
      }
      (scriptType, stdLibVersion, offset) = a
      scriptBytes                         = bytes.drop(offset).dropRight(checksumLength)

      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      s <- scriptType match {
        case ContentType.Expression =>
          for {
            _     <- ExprScript.validateBytes(scriptBytes)
            bytes <- Serde.deserialize(scriptBytes).map(_._1)
            s     <- ExprScript(stdLibVersion, bytes, checkSize = false)
          } yield s
        case ContentType.Contract =>
          for {
            bytes <- ContractSerDe.deserialize(scriptBytes)
            s     <- ContractScript(stdLibVersion, bytes)
          } yield s
      }
    } yield s).left
      .map(m => ScriptParseError(m.toString))
  }

}
