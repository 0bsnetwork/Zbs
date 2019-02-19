package com.zbsnetwork.state

import com.zbsnetwork.account.Address
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.crypto.SignatureLength
import com.zbsnetwork.db.WithDomain
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.transaction.GenesisTransaction
import com.zbsnetwork.{NoShrink, TestTime, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class CommonSpec extends FreeSpec with Matchers with WithDomain with TransactionGen with PropertyChecks with NoShrink {
  private val time          = new TestTime
  private def nextTs        = time.getTimestamp()
  private val AssetIdLength = 32

  private def genesisBlock(genesisTs: Long, address: Address, initialBalance: Long) = TestBlock.create(
    genesisTs,
    ByteStr(Array.fill[Byte](SignatureLength)(0)),
    Seq(GenesisTransaction.create(address, initialBalance, genesisTs).explicitGet())
  )

  "Common Conditions" - {
    "Zero balance of absent asset" in forAll(accountGen, positiveLongGen, byteArrayGen(AssetIdLength)) {
      case (sender, initialBalance, assetId) =>
        withDomain() { d =>
          d.appendBlock(genesisBlock(nextTs, sender, initialBalance))
          d.portfolio(sender).balanceOf(Some(ByteStr(assetId))) shouldEqual 0L
        }
    }
  }
}
