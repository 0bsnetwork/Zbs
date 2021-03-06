package com.zbsnetwork.utx

import java.nio.file.Files

import com.typesafe.config.ConfigFactory
import com.zbsnetwork._
import com.zbsnetwork.account.{Address, PrivateKeyAccount, PublicKeyAccount}
import com.zbsnetwork.block.Block
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.database.LevelDBWriter
import com.zbsnetwork.db.openDB
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.history.StorageFactory
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.lang.v1.compiler.Terms.EXPR
import com.zbsnetwork.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.zbsnetwork.mining._
import com.zbsnetwork.settings._
import com.zbsnetwork.state._
import com.zbsnetwork.state.diffs._
import com.zbsnetwork.transaction.ValidationError.SenderIsBlacklisted
import com.zbsnetwork.transaction.smart.SetScriptTransaction
import com.zbsnetwork.transaction.smart.script.Script
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.transfer.MassTransferTransaction.ParsedTransfer
import com.zbsnetwork.transaction.transfer._
import com.zbsnetwork.transaction.{AssetId, Transaction}
import com.zbsnetwork.utils.Implicits.SubjectOps
import com.zbsnetwork.utils.Time
import monix.reactive.subjects.Subject
import org.scalacheck.Gen
import org.scalacheck.Gen._
import org.scalamock.scalatest.MockFactory
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

import scala.concurrent.duration._

private object UtxPoolSpecification {
  private val ignoreSpendableBalanceChanged = Subject.empty[(Address, Option[AssetId])]

  final case class TempDB(fs: FunctionalitySettings) {
    val path   = Files.createTempDirectory("leveldb-test")
    val db     = openDB(path.toAbsolutePath.toString)
    val writer = new LevelDBWriter(db, ignoreSpendableBalanceChanged, fs, 100000, 2000, 120 * 60 * 1000)

    Runtime.getRuntime.addShutdownHook(new Thread(() => {
      db.close()
      TestHelpers.deleteRecursively(path)
    }))
  }
}

class UtxPoolSpecification extends FreeSpec with Matchers with MockFactory with PropertyChecks with TransactionGen with NoShrink {
  val PoolDefaultMaxBytes = 50 * 1024 * 1024 // 50 MB

  import CommonValidation.{ScriptExtraFee => extraFee}
  import FunctionalitySettings.TESTNET.{maxTransactionTimeBackOffset => maxAge}
  import UtxPoolSpecification._

  private def mkBlockchain(senderAccount: Address, senderBalance: Long) = {
    val config          = ConfigFactory.load()
    val genesisSettings = TestHelpers.genesisSettings(Map(senderAccount -> senderBalance))
    val origSettings    = ZbsSettings.fromConfig(config)
    val settings = origSettings.copy(
      blockchainSettings = BlockchainSettings(
        'T',
        FunctionalitySettings.TESTNET.copy(
          preActivatedFeatures = Map(
            BlockchainFeatures.MassTransfer.id  -> 0,
            BlockchainFeatures.SmartAccounts.id -> 0,
            BlockchainFeatures.Ride4DApps.id    -> 0
          )),
        genesisSettings
      ),
      featuresSettings = origSettings.featuresSettings.copy(autoShutdownOnUnsupportedFeature = false)
    )

    val dbContext = TempDB(settings.blockchainSettings.functionalitySettings)
    val bcu       = StorageFactory(settings, dbContext.db, new TestTime(), ignoreSpendableBalanceChanged)
    bcu.processBlock(Block.genesis(genesisSettings).explicitGet()).explicitGet()
    bcu
  }

  private def transfer(sender: PrivateKeyAccount, maxAmount: Long, time: Time) =
    (for {
      amount    <- chooseNum(1, (maxAmount * 0.9).toLong)
      recipient <- accountGen
      fee       <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransactionV1.selfSigned(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).explicitGet())
      .label("transferTransaction")

  private def transferWithRecipient(sender: PrivateKeyAccount, recipient: PublicKeyAccount, maxAmount: Long, time: Time) =
    (for {
      amount <- chooseNum(1, (maxAmount * 0.9).toLong)
      fee    <- chooseNum(extraFee, (maxAmount * 0.1).toLong)
    } yield TransferTransactionV1.selfSigned(None, sender, recipient, amount, time.getTimestamp(), None, fee, Array.empty[Byte]).explicitGet())
      .label("transferWithRecipient")

  private def massTransferWithRecipients(sender: PrivateKeyAccount, recipients: List[PublicKeyAccount], maxAmount: Long, time: Time) = {
    val amount    = maxAmount / (recipients.size + 1)
    val transfers = recipients.map(r => ParsedTransfer(r.toAddress, amount))
    val minFee    = CommonValidation.FeeConstants(TransferTransaction.typeId) + CommonValidation.FeeConstants(MassTransferTransaction.typeId) * transfers.size
    val txs = for { fee <- chooseNum(minFee, amount) } yield
      MassTransferTransaction.selfSigned(None, sender, transfers, time.getTimestamp(), fee, Array.empty[Byte]).explicitGet()
    txs.label("transferWithRecipient")
  }

  private val stateGen = for {
    sender        <- accountGen.label("sender")
    senderBalance <- positiveLongGen.label("senderBalance")
    if senderBalance > 100000L
  } yield {
    val bcu = mkBlockchain(sender, senderBalance)
    (sender, senderBalance, bcu)
  }

  private val twoOutOfManyValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    n                            <- chooseNum(3, 10)
    fee                          <- chooseNum(extraFee, (senderBalance * 0.01).toLong)
    offset                       <- chooseNum(1000L, 2000L)
  } yield {
    val time = new TestTime()
    val utx =
      new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        FunctionalitySettings.TESTNET,
        UtxSettings(10, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)
      )
    val amountPart = (senderBalance - fee) / 2 - fee
    val txs        = for (_ <- 1 to n) yield createZbsTransfer(sender, recipient, amountPart, fee, time.getTimestamp()).explicitGet()
    (utx, time, txs, (offset + 1000).millis)
  }).label("twoOutOfManyValidPayments")

  private val emptyUtxPool = stateGen
    .map {
      case (sender, _, bcu) =>
        val time = new TestTime()
        val utxPool =
          new UtxPoolImpl(
            time,
            bcu,
            ignoreSpendableBalanceChanged,
            FunctionalitySettings.TESTNET,
            UtxSettings(10, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)
          )
        (sender, bcu, utxPool)
    }
    .label("emptyUtxPool")

  private val withValidPayments = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time))
  } yield {
    val settings = UtxSettings(10, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)
    val utxPool  = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, FunctionalitySettings.TESTNET, settings)
    txs.foreach(utxPool.putIfNew)
    (sender, bcu, utxPool, time, settings)
  }).label("withValidPayments")

  private val withBlacklisted = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(10, PoolDefaultMaxBytes, Set(sender.address), Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, FunctionalitySettings.TESTNET, settings)
    (sender, utxPool, txs)
  }).label("withBlacklisted")

  private val withBlacklistedAndAllowedByRule = (for {
    (sender, senderBalance, bcu) <- stateGen
    recipient                    <- accountGen
    time = new TestTime()
    txs <- Gen.nonEmptyListOf(transferWithRecipient(sender, recipient, senderBalance / 10, time)) // @TODO: Random transactions
  } yield {
    val settings =
      UtxSettings(txs.length, PoolDefaultMaxBytes, Set(sender.address), Set(recipient.address), 5.minutes, allowTransactionsFromSmartAccounts = true)
    val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, FunctionalitySettings.TESTNET, settings)
    (sender, utxPool, txs)
  }).label("withBlacklistedAndAllowedByRule")

  private def massTransferWithBlacklisted(allowRecipients: Boolean) =
    (for {
      (sender, senderBalance, bcu) <- stateGen
      addressGen = Gen.listOf(accountGen).filter(list => if (allowRecipients) list.nonEmpty else true)
      recipients <- addressGen
      time = new TestTime()
      txs <- Gen.nonEmptyListOf(massTransferWithRecipients(sender, recipients, senderBalance / 10, time))
    } yield {
      val whitelist: Set[String] = if (allowRecipients) recipients.map(_.address).toSet else Set.empty
      val settings =
        UtxSettings(txs.length, PoolDefaultMaxBytes, Set(sender.address), whitelist, 5.minutes, allowTransactionsFromSmartAccounts = true)
      val utxPool = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, FunctionalitySettings.TESTNET, settings)
      (sender, utxPool, txs)
    }).label("massTransferWithBlacklisted")

  private def utxTest(utxSettings: UtxSettings =
                        UtxSettings(20, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true),
                      txCount: Int = 10)(f: (Seq[TransferTransactionV1], UtxPool, TestTime) => Unit): Unit =
    forAll(stateGen, chooseNum(2, txCount).label("txCount")) {
      case ((sender, senderBalance, bcu), count) =>
        val time = new TestTime()

        forAll(listOfN(count, transfer(sender, senderBalance / 2, time))) { txs =>
          val utx = new UtxPoolImpl(time, bcu, ignoreSpendableBalanceChanged, FunctionalitySettings.TESTNET, utxSettings)
          f(txs, utx, time)
        }
    }

  private val dualTxGen: Gen[(UtxPool, TestTime, Seq[Transaction], Seq[Transaction])] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      ts = System.currentTimeMillis()
      count1 <- chooseNum(5, 10)
      tx1    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts)))
      tx2    <- listOfN(count1, transfer(sender, senderBalance / 2, new TestTime(ts + maxAge.toMillis + 1000)))
    } yield {
      val time = new TestTime()
      val utx = new UtxPoolImpl(
        time,
        bcu,
        ignoreSpendableBalanceChanged,
        FunctionalitySettings.TESTNET,
        UtxSettings(10, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)
      )
      (utx, time, tx1, tx2)
    }

  private val expr: EXPR = {
    val code =
      """let x = 1
        |let y = 2
        |true""".stripMargin
    ExpressionCompiler.compile(code, CompilerContext.empty).explicitGet()
  }

  private val script: Script = ExprScript(expr).explicitGet()

  private def preconditionsGen(lastBlockId: ByteStr, master: PrivateKeyAccount): Gen[Seq[Block]] =
    for {
      version <- Gen.oneOf(SetScriptTransaction.supportedVersions.toSeq)
      ts      <- timestampGen
    } yield {
      val setScript = SetScriptTransaction.selfSigned(master, Some(script), 100000, ts + 1).explicitGet()
      Seq(TestBlock.create(ts + 1, lastBlockId, Seq(setScript)))
    }

  private def withScriptedAccount(scEnabled: Boolean): Gen[(PrivateKeyAccount, Long, UtxPoolImpl, Long)] =
    for {
      (sender, senderBalance, bcu) <- stateGen
      preconditions                <- preconditionsGen(bcu.lastBlockId.get, sender)
    } yield {
      val smartAccountsFs = TestFunctionalitySettings.Enabled.copy(preActivatedFeatures = Map(BlockchainFeatures.SmartAccounts.id -> 0))
      preconditions.foreach(b => bcu.processBlock(b).explicitGet())
      val utx = new UtxPoolImpl(
        new TestTime(),
        bcu,
        ignoreSpendableBalanceChanged,
        smartAccountsFs,
        UtxSettings(10, PoolDefaultMaxBytes, Set.empty, Set.empty, 1.day, allowTransactionsFromSmartAccounts = scEnabled)
      )

      (sender, senderBalance, utx, bcu.lastBlock.fold(0L)(_.timestamp))
    }

  private def transactionV1Gen(sender: PrivateKeyAccount, ts: Long, feeAmount: Long): Gen[TransferTransactionV1] = accountGen.map { recipient =>
    TransferTransactionV1.selfSigned(None, sender, recipient, zbs(1), ts, None, feeAmount, Array.emptyByteArray).explicitGet()
  }

  private def transactionV2Gen(sender: PrivateKeyAccount, ts: Long, feeAmount: Long): Gen[TransferTransactionV2] = accountGen.map { recipient =>
    TransferTransactionV2.selfSigned(None, sender, recipient, zbs(1), ts, None, feeAmount, Array.emptyByteArray).explicitGet()
  }

  "UTX Pool" - {
    "does not add new transactions when full" in utxTest(
      UtxSettings(1, PoolDefaultMaxBytes, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)) { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t))) should produce("pool size limit")
    }

    "does not add new transactions when full in bytes" in utxTest(
      UtxSettings(999999, 152, Set.empty, Set.empty, 5.minutes, allowTransactionsFromSmartAccounts = true)) { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      all(txs.tail.map(t => utx.putIfNew(t))) should produce("pool bytes size limit")
    }

    "does not broadcast the same transaction twice" in utxTest() { (txs, utx, _) =>
      utx.putIfNew(txs.head) shouldBe 'right
      utx.putIfNew(txs.head) shouldBe 'right
    }

    "evicts expired transactions when removeAll is called" in forAll(dualTxGen) {
      case (utx, time, txs1, txs2) =>
        all(txs1.map(utx.putIfNew)) shouldBe 'right
        utx.all.size shouldEqual txs1.size

        time.advance(maxAge + 1000.millis)
        utx.removeAll(Seq.empty)

        all(txs2.map(utx.putIfNew)) shouldBe 'right
        utx.all.size shouldEqual txs2.size
    }

    "packUnconfirmed result is limited by constraint" in forAll(dualTxGen) {
      case (utx, time, txs, _) =>
        all(txs.map(utx.putIfNew)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        val maxNumber             = Math.max(utx.all.size / 2, 3)
        val rest                  = limitByNumber(maxNumber)
        val (packed, restUpdated) = utx.packUnconfirmed(rest)

        packed.lengthCompare(maxNumber) should be <= 0
        if (maxNumber <= utx.all.size) restUpdated.isEmpty shouldBe true
    }

    "evicts expired transactions when packUnconfirmed is called" in forAll(dualTxGen) {
      case (utx, time, txs, _) =>
        all(txs.map(utx.putIfNew)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        time.advance(maxAge + 1000.millis)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100))
        packed shouldBe 'empty
        utx.all shouldBe 'empty
    }

    "evicts one of mutually invalid transactions when packUnconfirmed is called" in forAll(twoOutOfManyValidPayments) {
      case (utx, time, txs, offset) =>
        all(txs.map(utx.putIfNew)) shouldBe 'right
        utx.all.size shouldEqual txs.size

        time.advance(offset)

        val (packed, _) = utx.packUnconfirmed(limitByNumber(100))
        packed.size shouldBe 2
        utx.all.size shouldBe 2
    }

    "pessimisticPortfolio" - {
      "is not empty if there are transactions" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.size should be > 0
          utxPool.pessimisticPortfolio(sender) should not be empty
      }

      "is empty if there is no transactions" in forAll(emptyUtxPool) {
        case (sender, _, utxPool) =>
          utxPool.size shouldBe 0
          utxPool.pessimisticPortfolio(sender) shouldBe empty
      }

      "is empty if utx pool was cleaned" in forAll(withValidPayments) {
        case (sender, _, utxPool, _, _) =>
          utxPool.removeAll(utxPool.all)
          utxPool.pessimisticPortfolio(sender) shouldBe empty
      }

      "is changed after transactions with these assets are removed" in forAll(withValidPayments) {
        case (sender, _, utxPool, time, _) =>
          val portfolioBefore = utxPool.pessimisticPortfolio(sender)
          val poolSizeBefore  = utxPool.size

          time.advance(maxAge * 2)
          utxPool.packUnconfirmed(limitByNumber(100))

          poolSizeBefore should be > utxPool.size
          val portfolioAfter = utxPool.pessimisticPortfolio(sender)

          portfolioAfter should not be portfolioBefore
      }
    }

    "spendableBalance" - {
      "equal to state's portfolio if utx is empty" in forAll(emptyUtxPool) {
        case (sender, state, utxPool) =>
          val pessimisticAssetIds = {
            val p = utxPool.pessimisticPortfolio(sender)
            p.assetIds.filter(x => p.balanceOf(x) != 0)
          }

          pessimisticAssetIds shouldBe empty
      }

      "takes into account unconfirmed transactions" in forAll(withValidPayments) {
        case (sender, state, utxPool, _, _) =>
          val basePortfolio = state.portfolio(sender)
          val baseAssetIds  = basePortfolio.assetIds

          val pessimisticAssetIds = {
            val p = utxPool.pessimisticPortfolio(sender)
            p.assetIds.filter(x => p.balanceOf(x) != 0)
          }

          val unchangedAssetIds = baseAssetIds -- pessimisticAssetIds
          withClue("unchanged") {
            unchangedAssetIds.foreach { assetId =>
              basePortfolio.balanceOf(assetId) shouldBe basePortfolio.balanceOf(assetId)
            }
          }

          val changedAssetIds = pessimisticAssetIds -- baseAssetIds
          withClue("changed") {
            changedAssetIds.foreach { assetId =>
              basePortfolio.balanceOf(assetId) should not be basePortfolio.balanceOf(assetId)
            }
          }
      }
    }

    "blacklisting" - {
      "prevent a transfer transaction from specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklisted, massTransferWithBlacklisted(allowRecipients = false))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            val r = txs.forall { tx =>
              utxPool.putIfNew(tx) match {
                case Left(SenderIsBlacklisted(_)) => true
                case _                            => false
              }
            }

            r shouldBe true
            utxPool.all.size shouldEqual 0
        }
      }

      "allow a transfer transaction from blacklisted address to specific addresses" in {
        val transferGen = Gen.oneOf(withBlacklistedAndAllowedByRule, massTransferWithBlacklisted(allowRecipients = true))
        forAll(transferGen) {
          case (_, utxPool, txs) =>
            all(txs.map { t =>
              utxPool.putIfNew(t)
            }) shouldBe 'right
            utxPool.all.size shouldEqual txs.size
        }
      }
    }

    "smart accounts" - {
      "signed txs from scripted account is not allowed" in {
        val enoughFeeTxWithScriptedAccount =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(true)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx                               <- transactionV1Gen(sender, ts + 1, feeAmount)
          } yield (utx, tx)

        val (utx, tx) = enoughFeeTxWithScriptedAccount.sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
        utx.putIfNew(tx) should produce("signature from scripted account")
      }

      "any transaction from scripted account is not allowed if smartAccounts disabled in utx pool" - {

        def enoughFeeTxWithScriptedAccount(version: Int): Gen[(UtxPoolImpl, TransferTransaction)] =
          for {
            (sender, senderBalance, utx, ts) <- withScriptedAccount(false)
            feeAmount                        <- choose(extraFee, senderBalance / 2)
            tx <- version match {
              case 1 => transactionV1Gen(sender, ts + 1, feeAmount)
              case 2 => transactionV2Gen(sender, ts + 1, feeAmount)
            }
          } yield (utx, tx)

        "v1" in {
          val (utx1, tx1) = enoughFeeTxWithScriptedAccount(1).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx1.putIfNew(tx1) shouldBe 'left
        }
        "v2" in {
          val (utx2, tx2) = enoughFeeTxWithScriptedAccount(2).sample.getOrElse(throw new IllegalStateException("NO SAMPLE"))
          utx2.putIfNew(tx2) should produce("denied from UTX pool")
        }
      }
    }
  }

  private def limitByNumber(n: Int): MultiDimensionalMiningConstraint = MultiDimensionalMiningConstraint(
    OneDimensionalMiningConstraint(n, TxEstimators.one),
    OneDimensionalMiningConstraint(n, TxEstimators.one)
  )

}
