package com.zbsnetwork.lagonaki.unit

import com.zbsnetwork.account.PublicKeyAccount
import com.zbsnetwork.block.{Block, SignerData}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.consensus.nxt.NxtLikeConsensusBlockData
import com.zbsnetwork.metrics.Instrumented
import com.zbsnetwork.state.diffs.produce
import com.zbsnetwork.transaction._
import com.zbsnetwork.transaction.transfer._
import com.zbsnetwork.{NoShrink, TransactionGen, crypto}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.PropertyChecks

class BlockSpecification extends PropSpec with PropertyChecks with TransactionGen with Matchers with NoShrink {

  val time = System.currentTimeMillis() - 5000

  val blockGen = for {
    baseTarget          <- arbitrary[Long]
    reference           <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
    generationSignature <- byteArrayGen(Block.GeneratorSignatureLength)
    assetBytes          <- byteArrayGen(AssetIdLength)
    assetId = Some(ByteStr(assetBytes))
    sender                    <- accountGen
    recipient                 <- accountGen
    paymentTransaction        <- zbsTransferGeneratorP(time, sender, recipient)
    transferTrancation        <- transferGeneratorP(1 + time, sender, recipient, assetId, None)
    anotherPaymentTransaction <- zbsTransferGeneratorP(2 + time, sender, recipient)
    transactionData = Seq(paymentTransaction, transferTrancation, anotherPaymentTransaction)
  } yield (baseTarget, reference, ByteStr(generationSignature), recipient, transactionData)

  def bigBlockGen(amt: Int): Gen[Block] =
    for {
      baseTarget          <- arbitrary[Long]
      reference           <- byteArrayGen(Block.BlockIdLength).map(r => ByteStr(r))
      generationSignature <- byteArrayGen(Block.GeneratorSignatureLength)
      assetBytes          <- byteArrayGen(AssetIdLength)
      assetId = Some(ByteStr(assetBytes))
      sender                                    <- accountGen
      recipient                                 <- accountGen
      paymentTransaction: TransferTransactionV1 <- zbsTransferGeneratorP(time, sender, recipient)
    } yield
      Block
        .buildAndSign(3,
                      time,
                      reference,
                      NxtLikeConsensusBlockData(baseTarget, ByteStr(generationSignature)),
                      Seq.fill(amt)(paymentTransaction),
                      recipient,
                      Set.empty)
        .explicitGet()

  property(" block with txs bytes/parse roundtrip version 1,2") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          val block = Block
            .buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, Set.empty)
            .explicitGet()
          val parsedBlock = Block.parseBytes(block.bytes()).get
          assert(block.signaturesValid().isRight)
          assert(parsedBlock.signaturesValid().isRight)
          assert(parsedBlock.consensusData.generationSignature == generationSignature)
          assert(parsedBlock.version.toInt == version)
          assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
      }
    }
  }

  property(" block version 1,2 could not contain feature votes") {
    Seq[Byte](1, 2).foreach { version =>
      forAll(blockGen) {
        case (baseTarget, reference, generationSignature, recipient, transactionData) =>
          Block.buildAndSign(version, time, reference, NxtLikeConsensusBlockData(baseTarget, generationSignature), transactionData, recipient, Set(1)) should produce(
            "could not contain feature votes")
      }
    }
  }

  property(s" feature flags limit is ${Block.MaxFeaturesInBlock}") {
    val version           = 3.toByte
    val supportedFeatures = (0 to Block.MaxFeaturesInBlock * 2).map(_.toShort).toSet

    forAll(blockGen) {
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        Block.buildAndSign(version,
                           time,
                           reference,
                           NxtLikeConsensusBlockData(baseTarget, generationSignature),
                           transactionData,
                           recipient,
                           supportedFeatures) should produce(s"Block could not contain more than ${Block.MaxFeaturesInBlock} feature votes")
    }
  }
  property(" block with txs bytes/parse roundtrip version 3") {
    val version = 3.toByte

    val faetureSetGen: Gen[Set[Short]] = Gen.choose(0, Block.MaxFeaturesInBlock).flatMap(fc => Gen.listOfN(fc, arbitrary[Short])).map(_.toSet)

    forAll(blockGen, faetureSetGen) {
      case ((baseTarget, reference, generationSignature, recipient, transactionData), featureVotes) =>
        val block = Block
          .buildAndSign(version,
                        time,
                        reference,
                        NxtLikeConsensusBlockData(baseTarget, generationSignature),
                        transactionData,
                        recipient,
                        featureVotes)
          .explicitGet()
        val parsedBlock = Block.parseBytes(block.bytes()).get
        assert(block.signaturesValid().isRight)
        assert(parsedBlock.signaturesValid().isRight)
        assert(parsedBlock.consensusData.generationSignature == generationSignature)
        assert(parsedBlock.version.toInt == version)
        assert(parsedBlock.signerData.generator.publicKey.sameElements(recipient.publicKey))
        assert(parsedBlock.featureVotes == featureVotes)
    }
  }

  property("block signed by a weak public key is invalid") {
    val weakAccount = PublicKeyAccount(Array.fill(32)(0: Byte))
    forAll(blockGen) {
      case (baseTarget, reference, generationSignature, recipient, transactionData) =>
        val block = Block
          .build(
            3,
            time,
            reference,
            NxtLikeConsensusBlockData(baseTarget, generationSignature),
            transactionData,
            SignerData(weakAccount, ByteStr(Array.fill(64)(0: Byte))),
            Set.empty
          )
          .explicitGet()
        block.signaturesValid() shouldBe 'left
    }
  }

  ignore("sign time for 60k txs") {
    forAll(randomTransactionsGen(60000), accountGen, byteArrayGen(Block.BlockIdLength), byteArrayGen(Block.GeneratorSignatureLength)) {
      case ((txs, acc, ref, gs)) =>
        val (block, t0) =
          Instrumented.withTimeMillis(
            Block.buildAndSign(3, 1, ByteStr(ref), NxtLikeConsensusBlockData(1, ByteStr(gs)), txs, acc, Set.empty).explicitGet())
        val (bytes, t1) = Instrumented.withTimeMillis(block.bytesWithoutSignature())
        val (hash, t2)  = Instrumented.withTimeMillis(crypto.fastHash(bytes))
        val (sig, t3)   = Instrumented.withTimeMillis(crypto.sign(acc, hash))
        println((t0, t1, t2, t3))
    }
  }

  ignore("serialize and deserialize big block") {
    forAll(bigBlockGen(100 * 1000)) {
      case block =>
        val parsedBlock = Block.parseBytes(block.bytes()).get
        block.signaturesValid() shouldBe 'right
        parsedBlock.signaturesValid() shouldBe 'right
    }
  }
}
