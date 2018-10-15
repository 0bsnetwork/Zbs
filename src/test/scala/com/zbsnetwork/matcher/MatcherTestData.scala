package com.zbsplatform.matcher

import com.google.common.primitives.{Bytes, Ints}
import com.typesafe.config.ConfigFactory
import com.zbsplatform.account.PrivateKeyAccount
import com.zbsplatform.crypto
import com.zbsplatform.matcher.model.MatcherModel.Price
import com.zbsplatform.matcher.model.{BuyLimitOrder, SellLimitOrder}
import com.zbsplatform.settings.loadConfig
import com.zbsplatform.state.ByteStr
import com.zbsplatform.transaction.assets.exchange.{AssetPair, Order, OrderType}
import org.scalacheck.{Arbitrary, Gen}

trait MatcherTestData {
  private val signatureSize = 32

  val bytes32gen: Gen[Array[Byte]]       = Gen.listOfN(signatureSize, Arbitrary.arbitrary[Byte]).map(xs => xs.toArray)
  val WalletSeed                         = ByteStr("Matcher".getBytes())
  val MatcherSeed                        = crypto.secureHash(Bytes.concat(Ints.toByteArray(0), WalletSeed.arr))
  val MatcherAccount                     = PrivateKeyAccount(MatcherSeed)
  val accountGen: Gen[PrivateKeyAccount] = bytes32gen.map(seed => PrivateKeyAccount(seed))
  val positiveLongGen: Gen[Long]         = Gen.choose(1, Long.MaxValue)

  val zbsAssetGen: Gen[Option[Array[Byte]]] = Gen.const(None)

  def assetIdGen(prefix: Byte) = Gen.listOfN(signatureSize - 1, Arbitrary.arbitrary[Byte]).map(xs => Some(ByteStr(Array(prefix, xs: _*))))
  val distinctPairGen: Gen[AssetPair] = for {
    a1 <- assetIdGen(1.toByte)
    a2 <- assetIdGen(2.toByte)
  } yield AssetPair(a1, a2)

  val assetPairGen = Gen.frequency((18, distinctPairGen), (1, assetIdGen(1).map(AssetPair(_, None))), (1, assetIdGen(2).map(AssetPair(None, _))))

  val maxTimeGen: Gen[Long]     = Gen.choose(10000L, Order.MaxLiveTime).map(_ + System.currentTimeMillis())
  val createdTimeGen: Gen[Long] = Gen.choose(0L, 10000L).map(System.currentTimeMillis() - _)

  val config = loadConfig(ConfigFactory.parseString("""zbs {
      |  directory: "/tmp/zbs-test"
      |  matcher {
      |    enable: yes
      |    account: ""
      |    bind-address: "127.0.0.1"
      |    port: 6886
      |    order-history-file: null
      |    min-order-fee: 100000
      |    order-match-tx-fee: 100000
      |    snapshots-interval: 1d
      |    max-open-orders: 1000
      |    price-assets: ["BASE1", "BASE2", "BASE"]
      |    blacklisted-assets: ["BLACKLST"]
      |    blacklisted-names: ["[F,f]orbidden"]
      |  }
      |}""".stripMargin))

  val matcherSettings = MatcherSettings.fromConfig(config)

  def valueFromGen[T](gen: Gen[T]): T = {
    var value = gen.sample
    while (value.isEmpty) {
      value = gen.sample
    }
    value.get
  }

  val maxZbsAmountGen: Gen[Long] = Gen.choose(1, 100000000L * 100000000L)

  def buyGenerator(pair: AssetPair,
                   price: Long,
                   amount: Long,
                   sender: Option[PrivateKeyAccount] = None,
                   matcherFee: Option[Long] = None,
                   timestamp: Option[Long]): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxZbsAmountGen)
    } yield (Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def sellGenerator(pair: AssetPair,
                    price: Long,
                    amount: Long,
                    sender: Option[PrivateKeyAccount] = None,
                    matcherFee: Option[Long] = None,
                    timestamp: Option[Long]): Gen[(Order, PrivateKeyAccount)] =
    for {
      sender: PrivateKeyAccount <- sender.map(Gen.const).getOrElse(accountGen)
      timestamp: Long           <- timestamp.map(Gen.const).getOrElse(createdTimeGen)
      expiration: Long          <- maxTimeGen
      matcherFee: Long          <- matcherFee.map(Gen.const).getOrElse(maxZbsAmountGen)
    } yield (Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee), sender)

  def buy(pair: AssetPair,
          price: BigDecimal,
          amount: Long,
          sender: Option[PrivateKeyAccount] = None,
          matcherFee: Option[Long] = None,
          ts: Option[Long] = None): Order = rawBuy(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee, ts)

  def rawBuy(pair: AssetPair,
             price: Price,
             amount: Long,
             sender: Option[PrivateKeyAccount] = None,
             matcherFee: Option[Long] = None,
             ts: Option[Long] = None): Order =
    valueFromGen(buyGenerator(pair, price, amount, sender, matcherFee, ts))._1

  def sell(pair: AssetPair,
           price: BigDecimal,
           amount: Long,
           sender: Option[PrivateKeyAccount] = None,
           matcherFee: Option[Long] = None,
           ts: Option[Long] = None): Order = rawSell(pair, (price * Order.PriceConstant).toLong, amount, sender, matcherFee, ts)

  def rawSell(pair: AssetPair,
              price: Long,
              amount: Long,
              sender: Option[PrivateKeyAccount] = None,
              matcherFee: Option[Long] = None,
              ts: Option[Long] = None): Order =
    valueFromGen(sellGenerator(pair, price, amount, sender, matcherFee, ts))._1

  val orderTypeGenerator: Gen[OrderType] = Gen.oneOf(OrderType.BUY, OrderType.SELL)

  val orderGenerator: Gen[(Order, PrivateKeyAccount)] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    orderType                 <- orderTypeGenerator
    price: Long               <- maxZbsAmountGen
    amount: Long              <- maxZbsAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxZbsAmountGen
  } yield (Order(sender, MatcherAccount, pair, orderType, price, amount, timestamp, expiration, matcherFee), sender)

  val buyLimitOrderGenerator: Gen[BuyLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    price: Long               <- maxZbsAmountGen
    amount: Long              <- maxZbsAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxZbsAmountGen
  } yield BuyLimitOrder(price, amount, matcherFee, Order.buy(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

  val sellLimitOrderGenerator: Gen[SellLimitOrder] = for {
    sender: PrivateKeyAccount <- accountGen
    pair                      <- assetPairGen
    price: Long               <- maxZbsAmountGen
    amount: Long              <- maxZbsAmountGen
    timestamp: Long           <- createdTimeGen
    expiration: Long          <- maxTimeGen
    matcherFee: Long          <- maxZbsAmountGen
  } yield SellLimitOrder(price, amount, matcherFee, Order.sell(sender, MatcherAccount, pair, price, amount, timestamp, expiration, matcherFee))

}
