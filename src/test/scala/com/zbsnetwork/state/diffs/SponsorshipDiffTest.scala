package com.zbsnetwork.state.diffs

import com.zbsnetwork.TransactionGen
import com.zbsnetwork.common.utils.{Base58, EitherExt2}
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.lagonaki.mocks.TestBlock.{create => block}
import com.zbsnetwork.settings.{Constants, TestFunctionalitySettings}
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.GenesisTransaction
import com.zbsnetwork.transaction.assets.{IssueTransactionV1, SponsorFeeTransaction}
import com.zbsnetwork.transaction.lease.LeaseTransactionV1
import com.zbsnetwork.transaction.transfer._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class SponsorshipDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  def settings(sponsorshipActivationHeight: Int) =
    TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.FeeSponsorship.id -> sponsorshipActivationHeight),
                                           featureCheckBlocksPeriod = 1,
                                           blocksForFeatureActivation = 1)

  property("work") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (issueTx, sponsorTx, sponsor1Tx, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx, sponsor1Tx, cancelTx)

    forAll(setup) {
      case (genesis, issue, sponsor, sponsor1, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffAndState(setupBlocks, block(Seq(sponsor)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor.minSponsoredAssetFee.get))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe sponsor.minSponsoredAssetFee
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(sponsor1.minSponsoredAssetFee.get))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe sponsor1.minSponsoredAssetFee
        }
        assertDiffAndState(setupBlocks, block(Seq(sponsor, sponsor1, cancel)), s) {
          case (diff, state) =>
            diff.sponsorship shouldBe Map(sponsor.assetId -> SponsorshipValue(0))
            state.assetDescription(sponsor.assetId).map(_.sponsorship) shouldBe Some(0)
        }
    }
  }

  property("validation fails if asset doesn't exist") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (_, sponsorTx, _, cancelTx) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, sponsorTx, cancelTx)

    forAll(setup) {
      case (genesis, sponsor, cancel) =>
        val setupBlocks = Seq(block(Seq(genesis)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
        assertDiffEi(setupBlocks, block(Seq(cancel)), s) { blockDiffEi =>
          blockDiffEi should produce("Referenced assetId not found")
        }
    }
  }

  property("validation fails prior to feature activation") {
    val s = settings(100)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
    } yield (genesis, issueTx, sponsorTx)

    forAll(setup) {
      case (genesis, issue, sponsor) =>
        val setupBlocks = Seq(block(Seq(genesis, issue)))
        assertDiffEi(setupBlocks, block(Seq(sponsor)), s) { blockDiffEi =>
          blockDiffEi should produce("SponsorFeeTransaction has not been activated")
        }
    }
  }

  property("not enough fee") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      ts     <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      assetOverspend = TransferTransactionV1
        .selfSigned(None, master, recipient.toAddress, 1000000, ts + 1, Some(assetId), issueTx.quantity + 1, Array.emptyByteArray)
        .right
        .get
      insufficientFee = TransferTransactionV1
        .selfSigned(None, master, recipient.toAddress, 1000000, ts + 2, Some(assetId), sponsorTx.minSponsoredAssetFee.get - 1, Array.emptyByteArray)
        .right
        .get
      fee = 3000 * sponsorTx.minSponsoredAssetFee.get
      zbsOverspend = TransferTransactionV1
        .selfSigned(None, master, recipient.toAddress, 1000000, ts + 3, Some(assetId), fee, Array.emptyByteArray)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, assetOverspend, insufficientFee, zbsOverspend)

    forAll(setup) {
      case (genesis, issue, sponsor, assetOverspend, insufficientFee, zbsOverspend) =>
        val setupBlocks = Seq(block(Seq(genesis, issue, sponsor)))
        assertDiffEi(setupBlocks, block(Seq(assetOverspend)), s) { blockDiffEi =>
          blockDiffEi should produce("unavailable funds")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value of 100000 ZBS")
        }
        assertDiffEi(setupBlocks, block(Seq(zbsOverspend)), s) { blockDiffEi =>
          if (zbsOverspend.fee > issue.quantity)
            blockDiffEi should produce("unavailable funds")
          else
            blockDiffEi should produce("negative zbs balance")
        }
    }
  }

  property("not enough zbs to pay fee after leasing") {
    val s = settings(0)
    val setup = for {
      master <- accountGen
      alice  <- accountGen
      bob    <- accountGen
      ts     <- timestampGen
      fee    <- smallFeeGen
      amount                       = ENOUGH_AMT / 2
      genesis: GenesisTransaction  = GenesisTransaction.create(master, amount, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(bob, amount, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      assetId = issueTx.id()
      transferAssetTx: TransferTransactionV1 = TransferTransactionV1
        .selfSigned(Some(assetId), master, alice.toAddress, issueTx.quantity, ts + 2, None, fee, Array.emptyByteArray)
        .right
        .get
      leasingTx: LeaseTransactionV1 = LeaseTransactionV1
        .selfSigned(master, amount - issueTx.fee - sponsorTx.fee - 2 * fee, fee, ts + 3, bob)
        .right
        .get
      leasingToMasterTx: LeaseTransactionV1 = LeaseTransactionV1
        .selfSigned(bob, amount / 2, fee, ts + 3, master)
        .right
        .get
      insufficientFee = TransferTransactionV1
        .selfSigned(Some(assetId),
                    alice,
                    bob.toAddress,
                    issueTx.quantity / 12,
                    ts + 4,
                    Some(assetId),
                    sponsorTx.minSponsoredAssetFee.get,
                    Array.emptyByteArray)
        .right
        .get
    } yield (genesis, genesis2, issueTx, sponsorTx, transferAssetTx, leasingTx, insufficientFee, leasingToMasterTx)

    forAll(setup) {
      case (genesis, genesis2, issueTx, sponsorTx, transferAssetTx, leasingTx, insufficientFee, leasingToMaster) =>
        val setupBlocks = Seq(block(Seq(genesis, genesis2, issueTx, sponsorTx)), block(Seq(transferAssetTx, leasingTx)))
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("negative effective balance")
        }
        assertDiffEi(setupBlocks, block(Seq(leasingToMaster, insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("trying to spend leased money")
        }
    }
  }

  property("cannot cancel sponsorship") {
    val s = settings(0)
    val setup = for {
      master     <- accountGen
      notSponsor <- accountGen
      ts         <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(1, notSponsor, assetId, None, 1 * Constants.UnitsInWave, ts + 1)
        .right
        .get
      insufficientFee = SponsorFeeTransaction
        .selfSigned(1, notSponsor, assetId, None, 1 * Constants.UnitsInWave - 1, ts + 1)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee)

    forAll(setup) {
      case (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee) =>
        val setupBlocks = Seq(block(Seq(genesis, issueTx, sponsorTx)))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value of 100000000 ZBS: 99999999")
        }
    }
  }

  property("cannot сhange sponsorship fee") {
    val s = settings(0)
    val setup = for {
      master     <- accountGen
      notSponsor <- accountGen
      ts         <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 400000000, ts).explicitGet()
      (issueTx, sponsorTx, _, _) <- sponsorFeeCancelSponsorFeeGen(master)
      recipient                  <- accountGen
      assetId = issueTx.id()
      minFee <- smallFeeGen
      senderNotIssuer = SponsorFeeTransaction
        .selfSigned(1, notSponsor, assetId, Some(minFee), 1 * Constants.UnitsInWave, ts + 1)
        .right
        .get
      insufficientFee = SponsorFeeTransaction
        .selfSigned(1, master, assetId, Some(minFee), 1 * Constants.UnitsInWave - 1, ts + 1)
        .right
        .get
    } yield (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee)

    forAll(setup) {
      case (genesis, issueTx, sponsorTx, senderNotIssuer, insufficientFee) =>
        val setupBlocks = Seq(block(Seq(genesis, issueTx, sponsorTx)))
        assertDiffEi(setupBlocks, block(Seq(senderNotIssuer)), s) { blockDiffEi =>
          blockDiffEi should produce("Asset was issued by other address")
        }
        assertDiffEi(setupBlocks, block(Seq(insufficientFee)), s) { blockDiffEi =>
          blockDiffEi should produce("does not exceed minimal value of 100000000 ZBS: 99999999")
        }
    }
  }

  property("sponsor has no ZBS but receives them just in time") {
    val s = settings(0)
    val setup = for {
      master    <- accountGen
      recipient <- accountGen
      ts        <- timestampGen
      genesis: GenesisTransaction = GenesisTransaction.create(master, 300000000, ts).explicitGet()
      issue = IssueTransactionV1
        .selfSigned(master, Base58.decode("Asset").get, Array.emptyByteArray, 100, 2, reissuable = false, 100000000, ts + 1)
        .explicitGet()
      assetId = issue.id()
      sponsor = SponsorFeeTransaction.selfSigned(1, master, assetId, Some(100), 100000000, ts + 2).explicitGet()
      assetTransfer = TransferTransactionV1
        .selfSigned(Some(assetId), master, recipient, issue.quantity, ts + 3, None, 100000, Array.emptyByteArray)
        .right
        .get
      zbsTransfer = TransferTransactionV1
        .selfSigned(None, master, recipient, 99800000, ts + 4, None, 100000, Array.emptyByteArray)
        .right
        .get
      backZbsTransfer = TransferTransactionV1
        .selfSigned(None, recipient, master, 100000, ts + 5, Some(assetId), 100, Array.emptyByteArray)
        .right
        .get
    } yield (genesis, issue, sponsor, assetTransfer, zbsTransfer, backZbsTransfer)

    forAll(setup) {
      case (genesis, issue, sponsor, assetTransfer, zbsTransfer, backZbsTransfer) =>
        assertDiffAndState(Seq(block(Seq(genesis, issue, sponsor, assetTransfer, zbsTransfer))), block(Seq(backZbsTransfer)), s) {
          case (diff, state) =>
            val portfolio = state.portfolio(genesis.recipient)
            portfolio.balance shouldBe 0
            portfolio.assets(issue.id()) shouldBe issue.quantity
        }
    }
  }

}
