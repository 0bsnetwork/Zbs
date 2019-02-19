package com.zbsnetwork.mining

import com.zbsnetwork.TransactionGen
import com.zbsnetwork.account.{Address, PrivateKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.lang.StdLibVersion.V1
import com.zbsnetwork.lang.v1.compiler.Terms
import com.zbsnetwork.state.{AssetDescription, Blockchain}
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.transfer.TransferTransactionV1
import org.scalamock.scalatest.PathMockFactory
import org.scalatest.{FreeSpec, Matchers}

class TxEstimatorsSuite extends FreeSpec with Matchers with PathMockFactory with TransactionGen {
  "scriptRunNumber" - {
    "smart account" - {
      "should not count transactions going from a regular account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => false).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferZbsTx) shouldBe 0
      }

      "should count transactions going from a smart account" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => true).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferZbsTx) shouldBe 1
      }
    }

    "smart tokens" - {
      "should not count transactions working with a regular tokens" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => false).anyNumberOfTimes()
        (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => None).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferAssetsTx) shouldBe 0
      }

      "should count transactions working with smart tokens" in {
        val blockchain = stub[Blockchain]
        (blockchain.hasScript _).when(*).onCall((_: Address) => false).anyNumberOfTimes()
        (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => Some(assetDescription)).anyNumberOfTimes()

        TxEstimators.scriptRunNumber(blockchain, transferAssetsTx) shouldBe 1
      }
    }

    "both - should double count transactions working with smart tokens from samrt account" in {
      val blockchain = stub[Blockchain]
      (blockchain.hasScript _).when(*).onCall((_: Address) => true).anyNumberOfTimes()
      (blockchain.assetDescription _).when(*).onCall((_: ByteStr) => Some(assetDescription)).anyNumberOfTimes()

      TxEstimators.scriptRunNumber(blockchain, transferAssetsTx) shouldBe 2
    }
  }

  private val assetId = ByteStr("coin_id".getBytes())
  private val script  = ExprScript(V1, Terms.TRUE, checkSize = false).explicitGet()

  private val transferZbsTx = TransferTransactionV1
    .selfSigned(
      assetId = None,
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = None,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val transferAssetsTx = TransferTransactionV1
    .selfSigned(
      assetId = Some(assetId),
      sender = PrivateKeyAccount("sender".getBytes()),
      recipient = PrivateKeyAccount("recipient".getBytes()),
      amount = 1,
      timestamp = System.currentTimeMillis(),
      feeAssetId = None,
      feeAmount = 100000,
      attachment = Array.emptyByteArray
    )
    .explicitGet()

  private val assetDescription = AssetDescription(
    issuer = PrivateKeyAccount("sender".getBytes()),
    name = "coin".getBytes(),
    description = "description".getBytes(),
    decimals = 2,
    reissuable = false,
    totalVolume = Long.MaxValue,
    script = Some(script),
    sponsorship = 0
  )
}
