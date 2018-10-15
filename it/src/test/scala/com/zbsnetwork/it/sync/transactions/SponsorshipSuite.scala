package com.zbsplatform.it.sync.transactions

import com.typesafe.config.Config
import com.zbsplatform.api.http.assets.SignedSponsorFeeRequest
import com.zbsplatform.it.api.SyncHttpApi._
import com.zbsplatform.it.sync._
import com.zbsplatform.it.transactions.NodesFromDocker
import com.zbsplatform.it.util._
import com.zbsplatform.it.{NodeConfigs, ReportingTestName}
import com.zbsplatform.state.{ByteStr, Sponsorship}
import com.zbsplatform.transaction.assets.SponsorFeeTransaction
import com.zbsplatform.utils.Base58
import org.scalatest.{Assertion, CancelAfterFailure, FreeSpec, Matchers}
import play.api.libs.json.{JsNumber, JsObject, Json}

import scala.concurrent.duration._

class SponsorshipSuite extends FreeSpec with NodesFromDocker with Matchers with ReportingTestName with CancelAfterFailure {

  override def nodeConfigs: Seq[Config] =
    NodeConfigs.newBuilder
      .overrideBase(_.quorum(0))
      .overrideBase(_.raw("zbs.blockchain.custom.functionality.blocks-for-feature-activation=1"))
      .overrideBase(_.raw("zbs.blockchain.custom.functionality.feature-check-blocks-period=1"))
      .withDefault(1)
      .withSpecial(3, _.nonMiner)
      .buildNonConflicting()

  val miner             = nodes.head
  val sponsor           = nodes(1)
  val alice             = nodes(2)
  val bob               = nodes(3)
  val Zbs             = 100000000L
  val Token             = 100L
  val sponsorAssetTotal = 100 * Token
  val minSponsorFee     = Token
  val TinyFee           = Token / 2
  val SmallFee          = Token + Token / 2
  val LargeFee          = 10 * Token

  def assertMinAssetFee(txId: String, sponsorship: Long): Assertion = {
    val txInfo = miner.transactionInfo(txId)
    assert(txInfo.minSponsoredAssetFee.contains(sponsorship))
  }

  def assertSponsorship(assetId: String, sponsorship: Long): Assertion = {
    val assetInfo = miner.assetsDetails(assetId)
    assert(assetInfo.minSponsoredAssetFee == Some(sponsorship).filter(_ != 0))
  }

  "Fee in sponsored asset works fine" - {

    val sponsorZbsBalance = sponsor.accountBalances(sponsor.address)._2
    val minerZbsBalance   = miner.accountBalances(miner.address)._2

    val sponsorAssetId =
      sponsor
        .issue(sponsor.address, "SponsoredAsset", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = false, fee = issueFee)
        .id
    nodes.waitForHeightAriseAndTxPresent(sponsorAssetId)

    val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minFee, Some(sponsorAssetId), None).id
    nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

    val sponsorId = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, baseFee = Token, fee = sponsorFee).id

    "make asset sponsored" in {
      nodes.waitForHeightAriseAndTxPresent(sponsorId)
      assert(!sponsorAssetId.isEmpty)
      assert(!sponsorId.isEmpty)
      assertSponsorship(sponsorAssetId, 1 * Token)
      assertMinAssetFee(sponsorId, 1 * Token)
    }

    "check balance before test accounts balances" in {
      sponsor.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2)
      sponsor.assertBalances(sponsor.address, sponsorZbsBalance - 2 * issueFee - minFee)
      alice.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2)

      val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
    }

    "sender cannot make transfer" - {
      "invalid tx timestamp" in {

        def invalidTx(timestamp: Long) =
          SponsorFeeTransaction
            .selfSigned(1, sponsor.privateKey, ByteStr.decodeBase58(sponsorAssetId).get, Some(SmallFee), minFee, timestamp + 1.day.toMillis)
            .right
            .get

        def request(tx: SponsorFeeTransaction): SignedSponsorFeeRequest =
          SignedSponsorFeeRequest(
            tx.version,
            Base58.encode(tx.sender.publicKey),
            tx.assetId.base58,
            tx.minSponsoredAssetFee,
            tx.fee,
            tx.timestamp,
            tx.proofs.base58().toList
          )
        implicit val w =
          Json.writes[SignedSponsorFeeRequest].transform((jsobj: JsObject) => jsobj + ("type" -> JsNumber(SponsorFeeTransaction.typeId.toInt)))

        val iTx = invalidTx(timestamp = System.currentTimeMillis + 1.day.toMillis)
        assertBadRequestAndResponse(sponsor.broadcastRequest(request(iTx)), "Transaction .* is from far future")
      }
    }

    val minerZbsBalanceAfterFirstXferTest   = minerZbsBalance + 2.zbs + minFee + Sponsorship.FeeUnit * SmallFee / minSponsorFee
    val sponsorZbsBalanceAfterFirstXferTest = sponsorZbsBalance - 2.zbs - minFee - Sponsorship.FeeUnit * SmallFee / minSponsorFee

    "fee should be written off in issued asset" - {

      "alice transfer sponsored asset to bob using sponsored fee" in {
        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 10 * Token, SmallFee, Some(sponsorAssetId), Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        nodes.waitForHeightArise()

        assert(!transferTxCustomFeeAlice.isEmpty)
        miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - 10 * Token)
        miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)

        val aliceTx = alice.transactionsByAddress(alice.address, 100)
        aliceTx.head.size shouldBe 3
        aliceTx.head.count(tx => tx.sender.contains(alice.address) || tx.recipient.contains(alice.address)) shouldBe 3
        aliceTx.head.map(_.id) should contain allElementsOf Seq(transferTxCustomFeeAlice, transferTxToAlice)

        val bobTx = alice.transactionsByAddress(bob.address, 100)
        bobTx.head.size shouldBe 2
        bobTx.head.count(tx => tx.sender.contains(bob.address) || tx.recipient.contains(bob.address)) shouldBe 2
        bobTx.head.map(_.id) should contain(transferTxCustomFeeAlice)
      }

      "check transactions by address" in {
        val minerTx = miner.transactionsByAddress(miner.address, 100)
        minerTx.size shouldBe 1

        val sponsorTx = sponsor.transactionsByAddress(sponsor.address, 100)
//        sponsorTx.head.size shouldBe 4
//        sponsorTx.head.count(tx => tx.sender.contains(sponsor.address) || tx.recipient.contains(sponsor.address)) shouldBe 4
        sponsorTx.head.map(_.id) should contain allElementsOf Seq(sponsorId, transferTxToAlice, sponsorAssetId)
      }

      "sponsor should receive sponsored asset as fee, zbs should be written off" in {
        miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee)
        miner.assertBalances(sponsor.address, sponsorZbsBalanceAfterFirstXferTest)
      }

      "miner zbs balance should be changed" in {
        miner.assertBalances(miner.address, minerZbsBalanceAfterFirstXferTest)
      }
    }

    "assets balance should contain sponsor fee info and sponsor balance" in {
      val sponsorLeaseSomeZbs = sponsor.lease(sponsor.address, bob.address, leasingAmount, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorLeaseSomeZbs)
      val (_, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
      val assetsBalance                = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
      assetsBalance.minSponsoredAssetFee shouldBe Some(minSponsorFee)
      assetsBalance.sponsorBalance shouldBe Some(sponsorEffectiveBalance)
    }

    "zbs fee depends on sponsor fee and sponsored token decimals" in {
      val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.zbs, LargeFee, None, Some(sponsorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
      assert(!transferTxCustomFeeAlice.isEmpty)

      miner.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetTotal / 2 + SmallFee + LargeFee)
      miner.assertAssetBalance(alice.address, sponsorAssetId, sponsorAssetTotal / 2 - SmallFee - LargeFee - 10 * Token)
      miner.assertAssetBalance(bob.address, sponsorAssetId, 10 * Token)
      miner.assertBalances(
        sponsor.address,
        sponsorZbsBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee,
        sponsorZbsBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - leasingAmount
      )
      miner.assertBalances(miner.address, minerZbsBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token + leasingFee)
    }

    "cancel sponsorship" - {

      "cancel" in {
        val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = issueFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
        assert(!cancelSponsorshipTxId.isEmpty)
      }

      "check asset details info" in {
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe None
        assetInfo.sponsorBalance shouldBe None
      }

      "cannot pay fees in non sponsored assets" in {
        assertBadRequestAndResponse(
          alice.transfer(alice.address, bob.address, 10 * Token, fee = 1 * Token, assetId = None, feeAssetId = Some(sponsorAssetId)).id,
          s"Asset $sponsorAssetId is not sponsored, cannot be used to pay fees"
        )
      }

      "check cancel transaction info" in {
        assertSponsorship(sponsorAssetId, 0L)
      }

      "check sponsor and miner balances after cancel" in {
        miner.assertBalances(
          sponsor.address,
          sponsorZbsBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - issueFee,
          sponsorZbsBalanceAfterFirstXferTest - Sponsorship.FeeUnit * LargeFee / Token - leasingFee - leasingAmount - issueFee
        )
        miner.assertBalances(miner.address, minerZbsBalanceAfterFirstXferTest + Sponsorship.FeeUnit * LargeFee / Token + leasingFee + issueFee)
      }

      "cancel sponsopship again" in {
        val cancelSponsorshipTxId = sponsor.cancelSponsorship(sponsor.address, sponsorAssetId, fee = issueFee).id
        nodes.waitForHeightAriseAndTxPresent(cancelSponsorshipTxId)
        assert(!cancelSponsorshipTxId.isEmpty)
      }

    }
    "set sponsopship again" - {

      "set sponsorship and check new asset details, min sponsored fee changed" in {
        val setAssetSponsoredTx = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, fee = issueFee, baseFee = TinyFee).id
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx)
        assert(!setAssetSponsoredTx.isEmpty)
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe Some(Token / 2)
        assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
      }

      "make transfer with new min sponsored fee" in {
        val sponsoredBalance    = sponsor.accountBalances(sponsor.address)
        val sponsorAssetBalance = sponsor.assetBalance(sponsor.address, sponsorAssetId).balance
        val aliceAssetBalance   = alice.assetBalance(alice.address, sponsorAssetId).balance
        val aliceZbsBalance   = alice.accountBalances(alice.address)
        val bobAssetBalance     = bob.assetBalance(bob.address, sponsorAssetId).balance
        val bobZbsBalance     = bob.accountBalances(bob.address)
        val minerBalance        = miner.accountBalances(miner.address)
        val minerAssetBalance   = miner.assetBalance(miner.address, sponsorAssetId).balance

        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.zbs, TinyFee, None, Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        nodes.waitForHeightArise()

        val zbsFee = Sponsorship.FeeUnit * TinyFee / TinyFee
        sponsor.assertBalances(sponsor.address, sponsoredBalance._1 - zbsFee, sponsoredBalance._2 - zbsFee)
        sponsor.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetBalance + TinyFee)
        alice.assertAssetBalance(alice.address, sponsorAssetId, aliceAssetBalance - TinyFee)
        alice.assertBalances(alice.address, aliceZbsBalance._2 - 1.zbs)
        bob.assertBalances(bob.address, bobZbsBalance._1 + 1.zbs, bobZbsBalance._2 + 1.zbs)
        bob.assertAssetBalance(bob.address, sponsorAssetId, bobAssetBalance)
        miner.assertBalances(miner.address, minerBalance._2 + zbsFee)
        miner.assertAssetBalance(miner.address, sponsorAssetId, minerAssetBalance)
      }

      "change sponsorship fee in active sponsored asset" in {
        val setAssetSponsoredTx = sponsor.sponsorAsset(sponsor.address, sponsorAssetId, fee = issueFee, baseFee = LargeFee).id
        nodes.waitForHeightAriseAndTxPresent(setAssetSponsoredTx)
        nodes.waitForHeightArise()

        assert(!setAssetSponsoredTx.isEmpty)
        val assetInfo = alice.assetsBalance(alice.address).balances.filter(_.assetId == sponsorAssetId).head
        assetInfo.minSponsoredAssetFee shouldBe Some(LargeFee)
        assetInfo.sponsorBalance shouldBe Some(sponsor.accountBalances(sponsor.address)._2)
      }

      "transfer tx sponsored fee is less then new minimal" in {
        assertBadRequestAndResponse(
          sponsor
            .transfer(sponsor.address, alice.address, 10 * Token, fee = SmallFee, assetId = Some(sponsorAssetId), feeAssetId = Some(sponsorAssetId))
            .id,
          s"Fee in $sponsorAssetId .* does not exceed minimal value"
        )
      }

      "make transfer with updated min sponsored fee" in {
        val sponsoredBalance    = sponsor.accountBalances(sponsor.address)
        val sponsorAssetBalance = sponsor.assetBalance(sponsor.address, sponsorAssetId).balance
        val aliceAssetBalance   = alice.assetBalance(alice.address, sponsorAssetId).balance
        val aliceZbsBalance   = alice.accountBalances(alice.address)
        val bobZbsBalance     = bob.accountBalances(bob.address)
        val minerBalance        = miner.accountBalances(miner.address)

        val transferTxCustomFeeAlice = alice.transfer(alice.address, bob.address, 1.zbs, LargeFee, None, Some(sponsorAssetId)).id
        nodes.waitForHeightAriseAndTxPresent(transferTxCustomFeeAlice)
        val zbsFee = Sponsorship.FeeUnit * LargeFee / LargeFee
        nodes.waitForHeightArise()

        sponsor.assertBalances(sponsor.address, sponsoredBalance._1 - zbsFee, sponsoredBalance._2 - zbsFee)
        sponsor.assertAssetBalance(sponsor.address, sponsorAssetId, sponsorAssetBalance + LargeFee)
        alice.assertAssetBalance(alice.address, sponsorAssetId, aliceAssetBalance - LargeFee)
        alice.assertBalances(alice.address, aliceZbsBalance._2 - 1.zbs)
        bob.assertBalances(bob.address, bobZbsBalance._1 + 1.zbs, bobZbsBalance._2 + 1.zbs)
        miner.assertBalances(miner.address, minerBalance._2 + zbsFee)
      }

    }

    "issue asset make sponsor and burn and reissue" in {
      val sponsorBalance = sponsor.accountBalances(sponsor.address)
      val minerBalance   = miner.accountBalances(miner.address)

      val sponsorAssetId2 =
        sponsor
          .issue(sponsor.address, "Another", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 2, reissuable = true, fee = issueFee)
          .id
      nodes.waitForHeightAriseAndTxPresent(sponsorAssetId2)
      sponsor.sponsorAsset(sponsor.address, sponsorAssetId2, baseFee = Token, fee = sponsorFee).id
      val transferTxToAlice = sponsor.transfer(sponsor.address, alice.address, sponsorAssetTotal / 2, minFee, Some(sponsorAssetId2), None).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)

      val burnTxId = sponsor.burn(sponsor.address, sponsorAssetId2, sponsorAssetTotal / 2, burnFee).id
      nodes.waitForHeightAriseAndTxPresent(burnTxId)

      val assetInfo = sponsor.assetsDetails(sponsorAssetId2)
      assetInfo.minSponsoredAssetFee shouldBe Some(Token)
      assetInfo.quantity shouldBe sponsorAssetTotal / 2

      val sponsorAssetId2Reissue = sponsor.reissue(sponsor.address, sponsorAssetId2, sponsorAssetTotal, true, issueFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorAssetId2Reissue)

      val assetInfoAfterReissue = sponsor.assetsDetails(sponsorAssetId2)
      assetInfoAfterReissue.minSponsoredAssetFee shouldBe Some(Token)
      assetInfoAfterReissue.quantity shouldBe sponsorAssetTotal / 2 + sponsorAssetTotal
      assetInfoAfterReissue.reissuable shouldBe true

      val aliceTransferZbs = alice.transfer(alice.address, bob.address, transferAmount, SmallFee, None, Some(sponsorAssetId2)).id
      nodes.waitForHeightAriseAndTxPresent(aliceTransferZbs)
      nodes.waitForHeightArise()

      val totalZbsFee = Sponsorship.FeeUnit * SmallFee / Token + issueFee + sponsorFee + burnFee + minFee + issueFee
      miner.assertBalances(miner.address, minerBalance._1 + totalZbsFee)
      sponsor.assertBalances(sponsor.address, sponsorBalance._1 - totalZbsFee, sponsorBalance._2 - totalZbsFee)
      sponsor.assertAssetBalance(sponsor.address, sponsorAssetId2, SmallFee + sponsorAssetTotal)
    }

    "miner is sponsor" in {
      val minerBalance = miner.accountBalances(miner.address)
      val minersSpondorAssetId =
        miner
          .issue(miner.address, "MinersAsset", "Created by Sponsorship Suite", sponsorAssetTotal, decimals = 8, reissuable = true, fee = issueFee)
          .id
      nodes.waitForHeightAriseAndTxPresent(minersSpondorAssetId)
      val makeAssetSponsoredTx = miner.sponsorAsset(miner.address, minersSpondorAssetId, baseFee = Token, fee = sponsorFee).id
      nodes.waitForHeightAriseAndTxPresent(makeAssetSponsoredTx)
      val transferTxToAlice =
        miner.transfer(miner.address, alice.address, sponsorAssetTotal / 2, SmallFee, Some(minersSpondorAssetId), Some(minersSpondorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(transferTxToAlice)
      nodes.waitForHeightArise()

      miner.assertBalances(miner.address, minerBalance._1)
      val aliceSponsoredTransferZbs = alice.transfer(alice.address, bob.address, transferAmount, SmallFee, None, Some(minersSpondorAssetId)).id
      nodes.waitForHeightAriseAndTxPresent(aliceSponsoredTransferZbs)
      nodes.waitForHeightArise()

      miner.assertBalances(miner.address, minerBalance._1)
      miner.assertAssetBalance(miner.address, minersSpondorAssetId, sponsorAssetTotal / 2 + SmallFee)
    }

    "tx is declined if sponsor has not enough effective balance to pay fee" in {
      val (sponsorBalance, sponsorEffectiveBalance) = sponsor.accountBalances(sponsor.address)
      val sponsorLeaseAllAvaliableZbs             = sponsor.lease(sponsor.address, bob.address, sponsorEffectiveBalance - leasingFee, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(sponsorLeaseAllAvaliableZbs)
      assertBadRequestAndMessage(alice.transfer(alice.address, bob.address, 10 * Token, LargeFee, Some(sponsorAssetId), Some(sponsorAssetId)),
                                 "unavailable funds")
      val cancelLeasingTx = sponsor.cancelLease(sponsor.address, sponsorLeaseAllAvaliableZbs, leasingFee).id
      nodes.waitForHeightAriseAndTxPresent(cancelLeasingTx)
    }
  }

}
