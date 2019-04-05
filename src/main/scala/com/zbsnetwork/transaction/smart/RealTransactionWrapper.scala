package com.zbsnetwork.transaction.smart

import com.zbsnetwork.account.{Address, AddressOrAlias, Alias}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.lang.v1.traits.domain.Tx.{Header, Proven}
import com.zbsnetwork.lang.v1.traits.domain._
import com.zbsnetwork.state._
import com.zbsnetwork.transaction._
import com.zbsnetwork.transaction.assets._
import com.zbsnetwork.transaction.assets.exchange.OrderType.{BUY, SELL}
import com.zbsnetwork.transaction.assets.exchange.{AssetPair, ExchangeTransaction, Order}
import com.zbsnetwork.transaction.lease.{LeaseCancelTransaction, LeaseTransaction}
import com.zbsnetwork.transaction.transfer._

object RealTransactionWrapper {

  private def header(tx: Transaction): Header = {
    val v = tx match {
      case vt: VersionedTransaction => vt.version
      case _                        => 1
    }
    Header(ByteStr(tx.id().arr), tx.assetFee._2, tx.timestamp, v)
  }
  private def proven(tx: ProvenTransaction): Proven =
    Proven(
      header(tx),
      Recipient.Address(ByteStr(tx.sender.bytes.arr)),
      ByteStr(tx.bodyBytes()),
      ByteStr(tx.sender.publicKey),
      tx.proofs.proofs.map(_.arr).map(ByteStr(_)).toIndexedSeq
    )

  implicit def assetPair(a: AssetPair): APair = APair(a.amountAsset, a.priceAsset)
  implicit def ord(o: Order): Ord =
    Ord(
      id = ByteStr(o.id.value.arr),
      sender = Recipient.Address(ByteStr(o.sender.bytes.arr)),
      senderPublicKey = ByteStr(o.senderPublicKey.publicKey),
      matcherPublicKey = ByteStr(o.matcherPublicKey.publicKey),
      assetPair = o.assetPair,
      orderType = o.orderType match {
        case BUY  => OrdType.Buy
        case SELL => OrdType.Sell
      },
      amount = o.amount,
      price = o.price,
      timestamp = o.timestamp,
      expiration = o.expiration,
      matcherFee = o.matcherFee,
      bodyBytes = ByteStr(o.bodyBytes()),
      proofs = o.proofs.proofs.map(a => ByteStr(a.arr)).toIndexedSeq
    )

  implicit def aoaToRecipient(aoa: AddressOrAlias): Recipient = aoa match {
    case a: Address => Recipient.Address(ByteStr(a.bytes.arr))
    case a: Alias   => Recipient.Alias(a.name)
  }

  def apply(tx: Transaction): Tx = {
    tx match {
      case g: GenesisTransaction => Tx.Genesis(header(g), g.amount, g.recipient)
      case t: TransferTransaction =>
        Tx.Transfer(
          proven(t),
          feeAssetId = t.feeAssetId,
          assetId = t.assetId,
          amount = t.amount,
          recipient = t.recipient,
          attachment = ByteStr(t.attachment)
        )
      case i: IssueTransaction =>
        Tx.Issue(proven(i), i.quantity, ByteStr(i.name), ByteStr(i.description), i.reissuable, i.decimals, i.script.map(_.bytes()))
      case r: ReissueTransaction     => Tx.ReIssue(proven(r), r.quantity, r.assetId, r.reissuable)
      case b: BurnTransaction        => Tx.Burn(proven(b), b.quantity, b.assetId)
      case b: LeaseTransaction       => Tx.Lease(proven(b), b.amount, b.recipient)
      case b: LeaseCancelTransaction => Tx.LeaseCancel(proven(b), b.leaseId)
      case b: CreateAliasTransaction => Tx.CreateAlias(proven(b), b.alias.name)
      case ms: MassTransferTransaction =>
        Tx.MassTransfer(
          proven(ms),
          assetId = ms.assetId.map(a => ByteStr(a.arr)),
          transferCount = ms.transfers.length,
          totalAmount = ms.transfers.map(_.amount).sum,
          transfers = ms.transfers.map(r => com.zbsnetwork.lang.v1.traits.domain.Tx.TransferItem(r.address, r.amount)).toIndexedSeq,
          attachment = ByteStr(ms.attachment)
        )
      case ss: SetScriptTransaction      => Tx.SetScript(proven(ss), ss.script.map(_.bytes()))
      case ss: SetAssetScriptTransaction => Tx.SetAssetScript(proven(ss), ss.assetId, ss.script.map(_.bytes()))
      case p: PaymentTransaction         => Tx.Payment(proven(p), p.amount, p.recipient)
      case e: ExchangeTransaction        => Tx.Exchange(proven(e), e.amount, e.price, e.buyMatcherFee, e.sellMatcherFee, e.buyOrder, e.sellOrder)
      case s: SponsorFeeTransaction      => Tx.Sponsorship(proven(s), s.assetId, s.minSponsoredAssetFee)
      case d: DataTransaction =>
        Tx.Data(
          proven(d),
          d.data.map {
            case IntegerDataEntry(key, value) => DataItem.Lng(key, value)
            case StringDataEntry(key, value)  => DataItem.Str(key, value)
            case BooleanDataEntry(key, value) => DataItem.Bool(key, value)
            case BinaryDataEntry(key, value)  => DataItem.Bin(key, value)
          }.toIndexedSeq
        )
    }
  }
}
