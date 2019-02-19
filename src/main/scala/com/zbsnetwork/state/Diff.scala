package com.zbsnetwork.state

import cats.implicits._
import cats.kernel.Monoid
import com.zbsnetwork.account.{Address, Alias, PublicKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.features.FeatureProvider._
import com.zbsnetwork.settings.FunctionalitySettings
import com.zbsnetwork.state.diffs.CommonValidation
import com.zbsnetwork.transaction.smart.script.Script
import com.zbsnetwork.transaction.{AssetId, Transaction}

case class LeaseBalance(in: Long, out: Long)

object LeaseBalance {
  val empty = LeaseBalance(0, 0)

  implicit val m: Monoid[LeaseBalance] = new Monoid[LeaseBalance] {
    override def empty: LeaseBalance = LeaseBalance.empty

    override def combine(x: LeaseBalance, y: LeaseBalance): LeaseBalance =
      LeaseBalance(safeSum(x.in, y.in), safeSum(x.out, y.out))
  }
}

case class VolumeAndFee(volume: Long, fee: Long)

object VolumeAndFee {
  val empty = VolumeAndFee(0, 0)

  implicit val m: Monoid[VolumeAndFee] = new Monoid[VolumeAndFee] {
    override def empty: VolumeAndFee = VolumeAndFee.empty

    override def combine(x: VolumeAndFee, y: VolumeAndFee): VolumeAndFee =
      VolumeAndFee(x.volume + y.volume, x.fee + y.fee)
  }
}

case class AssetInfo(isReissuable: Boolean, volume: BigInt)
object AssetInfo {
  implicit val assetInfoMonoid: Monoid[AssetInfo] = new Monoid[AssetInfo] {
    override def empty: AssetInfo = AssetInfo(isReissuable = true, 0)
    override def combine(x: AssetInfo, y: AssetInfo): AssetInfo =
      AssetInfo(x.isReissuable && y.isReissuable, x.volume + y.volume)
  }
}

case class AssetDescription(issuer: PublicKeyAccount,
                            name: Array[Byte],
                            description: Array[Byte],
                            decimals: Int,
                            reissuable: Boolean,
                            totalVolume: BigInt,
                            script: Option[Script],
                            sponsorship: Long) {
  override def equals(obj: scala.Any) = obj match {
    case o: AssetDescription =>
      o.issuer == this.issuer &&
        o.name.sameElements(name) &&
        o.description.sameElements(description) &&
        o.decimals == decimals &&
        o.reissuable == reissuable &&
        o.totalVolume == totalVolume &&
        o.script == script &&
        o.sponsorship == sponsorship
    case _ => false
  }
}

case class AccountDataInfo(data: Map[String, DataEntry[_]])

object AccountDataInfo {
  implicit val accountDataInfoMonoid: Monoid[AccountDataInfo] = new Monoid[AccountDataInfo] {
    override def empty: AccountDataInfo = AccountDataInfo(Map.empty)

    override def combine(x: AccountDataInfo, y: AccountDataInfo): AccountDataInfo = AccountDataInfo(x.data ++ y.data)
  }
}

sealed abstract class Sponsorship
case class SponsorshipValue(minFee: Long) extends Sponsorship
case object SponsorshipNoInfo             extends Sponsorship

object Sponsorship {
  implicit val sponsorshipMonoid: Monoid[Sponsorship] = new Monoid[Sponsorship] {
    override def empty: Sponsorship = SponsorshipNoInfo

    override def combine(x: Sponsorship, y: Sponsorship): Sponsorship = y match {
      case SponsorshipNoInfo => x
      case _                 => y
    }
  }

  def sponsoredFeesSwitchHeight(blockchain: Blockchain, fs: FunctionalitySettings): Int =
    blockchain
      .featureActivationHeight(BlockchainFeatures.FeeSponsorship.id)
      .map(h => h + fs.activationWindowSize(h))
      .getOrElse(Int.MaxValue)

  def toZbs(assetFee: Long, sponsorship: Long): Long = {
    val zbs = (BigDecimal(assetFee) * BigDecimal(CommonValidation.FeeUnit)) / BigDecimal(sponsorship)
    if (zbs > Long.MaxValue) {
      throw new java.lang.ArithmeticException("Overflow")
    }
    zbs.toLong
  }

  def fromZbs(zbsFee: Long, sponsorship: Long): Long = {
    val assetFee = (BigDecimal(zbsFee) / BigDecimal(CommonValidation.FeeUnit)) * BigDecimal(sponsorship)
    if (assetFee > Long.MaxValue) {
      throw new java.lang.ArithmeticException("Overflow")
    }
    assetFee.toLong
  }
}

case class Diff(transactions: Map[ByteStr, (Int, Transaction, Set[Address])],
                portfolios: Map[Address, Portfolio],
                issuedAssets: Map[AssetId, AssetInfo],
                aliases: Map[Alias, Address],
                orderFills: Map[ByteStr, VolumeAndFee],
                leaseState: Map[ByteStr, Boolean],
                scripts: Map[Address, Option[Script]],
                assetScripts: Map[AssetId, Option[Script]],
                accountData: Map[Address, AccountDataInfo],
                sponsorship: Map[AssetId, Sponsorship])

object Diff {

  def stateOps(portfolios: Map[Address, Portfolio] = Map.empty,
               assetInfos: Map[AssetId, AssetInfo] = Map.empty,
               aliases: Map[Alias, Address] = Map.empty,
               orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
               leaseState: Map[ByteStr, Boolean] = Map.empty,
               scripts: Map[Address, Option[Script]] = Map.empty,
               assetScripts: Map[AssetId, Option[Script]] = Map.empty,
               accountData: Map[Address, AccountDataInfo] = Map.empty,
               sponsorship: Map[AssetId, Sponsorship] = Map.empty): Diff =
    Diff(
      transactions = Map(),
      portfolios = portfolios,
      issuedAssets = assetInfos,
      aliases = aliases,
      orderFills = orderFills,
      leaseState = leaseState,
      scripts = scripts,
      assetScripts = assetScripts,
      accountData = accountData,
      sponsorship = sponsorship
    )

  def apply(height: Int,
            tx: Transaction,
            portfolios: Map[Address, Portfolio] = Map.empty,
            assetInfos: Map[AssetId, AssetInfo] = Map.empty,
            aliases: Map[Alias, Address] = Map.empty,
            orderFills: Map[ByteStr, VolumeAndFee] = Map.empty,
            leaseState: Map[ByteStr, Boolean] = Map.empty,
            scripts: Map[Address, Option[Script]] = Map.empty,
            assetScripts: Map[AssetId, Option[Script]] = Map.empty,
            accountData: Map[Address, AccountDataInfo] = Map.empty,
            sponsorship: Map[AssetId, Sponsorship] = Map.empty): Diff =
    Diff(
      transactions = Map((tx.id(), (height, tx, portfolios.keys.toSet))),
      portfolios = portfolios,
      issuedAssets = assetInfos,
      aliases = aliases,
      orderFills = orderFills,
      leaseState = leaseState,
      scripts = scripts,
      assetScripts = assetScripts,
      accountData = accountData,
      sponsorship = sponsorship
    )

  val empty = new Diff(Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty, Map.empty)

  implicit val diffMonoid = new Monoid[Diff] {
    override def empty: Diff = Diff.empty

    override def combine(older: Diff, newer: Diff): Diff =
      Diff(
        transactions = older.transactions ++ newer.transactions,
        portfolios = older.portfolios.combine(newer.portfolios),
        issuedAssets = older.issuedAssets.combine(newer.issuedAssets),
        aliases = older.aliases ++ newer.aliases,
        orderFills = older.orderFills.combine(newer.orderFills),
        leaseState = older.leaseState ++ newer.leaseState,
        scripts = older.scripts ++ newer.scripts,
        assetScripts = older.assetScripts ++ newer.assetScripts,
        accountData = older.accountData.combine(newer.accountData),
        sponsorship = older.sponsorship.combine(newer.sponsorship)
      )
  }
}
