package com.zbsplatform.transaction.smart.script

import com.zbsplatform.crypto
import com.zbsplatform.lang.ScriptVersion
import com.zbsplatform.lang.ScriptVersion.Versions.V1
import com.zbsplatform.lang.v1.Serde
import com.zbsplatform.transaction.ValidationError.ScriptParseError
import com.zbsplatform.transaction.smart.script.v1.ScriptV1

object ScriptReader {

  val checksumLength = 4

  def fromBytes(bytes: Array[Byte]): Either[ScriptParseError, Script] = {
    val checkSum         = bytes.takeRight(checksumLength)
    val computedCheckSum = crypto.secureHash(bytes.dropRight(checksumLength)).take(checksumLength)
    val version          = bytes.head
    val scriptBytes      = bytes.drop(1).dropRight(checksumLength)

    for {
      _ <- Either.cond(checkSum.sameElements(computedCheckSum), (), ScriptParseError("Invalid checksum"))
      sv <- ScriptVersion
        .fromInt(version)
        .fold[Either[ScriptParseError, ScriptVersion]](Left(ScriptParseError(s"Invalid version: $version")))(v => Right(v))
      script <- sv match {
        case V1 =>
          ScriptV1
            .validateBytes(scriptBytes)
            .flatMap { _ =>
              Serde.deserialize(scriptBytes).flatMap(ScriptV1(_, checkSize = false))
            }
            .left
            .map(ScriptParseError)
      }
    } yield script
  }

}
