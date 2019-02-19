package com.zbsnetwork.lang.v1

import com.zbsnetwork.lang.contract.{Contract, ContractSerDe}
import com.zbsnetwork.lang.v1.compiler.Terms.EXPR
import com.zbsnetwork.lang.v1.compiler.{CompilerContext, ContractCompiler, ExpressionCompiler, Terms}

/**
  * This is a hack class for IDEA. The Global class is in JS/JVM modules.
  * And IDEA can't find the Global class in the "shared" module, but it must!
  */
trait BaseGlobal {
  val MaxBase58Bytes   = 64
  val MaxBase58String  = 100
  val MaxBase64Bytes   = 32 * 1024
  val MaxBase64String  = 44 * 1024
  val MaxLiteralLength = 12 * 1024
  val MaxAddressLength = 36

  def base58Encode(input: Array[Byte]): Either[String, String]
  def base58Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def base64Encode(input: Array[Byte]): Either[String, String]
  def base64Decode(input: String, limit: Int = MaxLiteralLength): Either[String, Array[Byte]]

  def curve25519verify(message: Array[Byte], sig: Array[Byte], pub: Array[Byte]): Boolean

  def keccak256(message: Array[Byte]): Array[Byte]
  def blake2b256(message: Array[Byte]): Array[Byte]
  def sha256(message: Array[Byte]): Array[Byte]

  def secureHash(a: Array[Byte]): Array[Byte] = keccak256(blake2b256(a))

  def checksum(arr: Array[Byte]): Array[Byte] = secureHash(arr).take(4)

  def compileScript(input: String, context: CompilerContext): Either[String, (Array[Byte], Terms.EXPR)] = {

    def serialize(expr: EXPR): Either[String, Array[Byte]] = {
      val s = 1.toByte +: Serde.serialize(expr)
      Right(s ++ checksum(s))
    }

    ExpressionCompiler
      .compile(input, context)
      .flatMap(ast => serialize(ast).map(x => (x, ast)))
  }

  def compileContract(input: String, ctx: CompilerContext): Either[String, (Array[Byte], Contract)] = {

    def serialize(expr: Contract): Either[String, Array[Byte]] = {
      val s = 3.toByte +: ContractSerDe.serialize(expr)
      Right(s ++ checksum(s))
    }

    ContractCompiler
      .compile(input, ctx)
      .flatMap(ast => serialize(ast).map(x => (x, ast)))
  }
}
