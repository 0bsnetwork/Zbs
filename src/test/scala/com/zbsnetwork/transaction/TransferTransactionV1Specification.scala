package com.zbsplatform.transaction

import com.zbsplatform.TransactionGen
import com.zbsplatform.state.{ByteStr, EitherExt2}
import org.scalatest._
import org.scalatest.prop.PropertyChecks
import play.api.libs.json.Json
import com.zbsplatform.account.{Address, PublicKeyAccount}
import com.zbsplatform.transaction.transfer._
import com.zbsplatform.state.diffs._
import com.zbsplatform.utils.Base58

class TransferTransactionV1Specification extends PropSpec with PropertyChecks with Matchers with TransactionGen {

  property("Transfer serialization roundtrip") {
    forAll(transferV1Gen) { transfer: TransferTransactionV1 =>
      val recovered = TransferTransactionV1.parseBytes(transfer.bytes()).get

      recovered.sender.address shouldEqual transfer.sender.address
      recovered.assetId.map(_ == transfer.assetId.get).getOrElse(transfer.assetId.isEmpty) shouldBe true
      recovered.feeAssetId.map(_ == transfer.feeAssetId.get).getOrElse(transfer.feeAssetId.isEmpty) shouldBe true
      recovered.timestamp shouldEqual transfer.timestamp
      recovered.amount shouldEqual transfer.amount
      recovered.fee shouldEqual transfer.fee
      recovered.recipient.stringRepr shouldEqual transfer.recipient.stringRepr

      recovered.bytes() shouldEqual transfer.bytes()
    }
  }

  property("Transfer serialization from TypedTransaction") {
    forAll(transferV1Gen) { tx: TransferTransactionV1 =>
      val recovered = TransactionParsers.parseBytes(tx.bytes()).get
      recovered.bytes() shouldEqual tx.bytes()
    }
  }

  property("JSON format validation") {
    val js = Json.parse("""{
                        "type": 4,
                        "id": "FLszEaqasJptohmP6zrXodBwjaEYq4jRP2BzdPPjvukk",
                        "sender": "3N5GRqzDBhjVXnCn44baHcz2GoZy5qLxtTh",
                        "senderPublicKey": "FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z",
                        "fee": 100000,
                        "timestamp": 1526552510868,
                        "signature": "eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz",
                        "version": 1,
                        "recipient": "3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8",
                        "assetId": null,
                        "feeAssetId": null,
                        "feeAsset": null,
                        "amount": 1900000,
                        "attachment": "4t2Xazb2SX"
                        }
    """)

    val tx = TransferTransactionV1
      .create(
        None,
        PublicKeyAccount.fromBase58String("FM5ojNqW7e9cZ9zhPYGkpSP1Pcd8Z3e3MNKYVS5pGJ8Z").explicitGet(),
        Address.fromString("3My3KZgFQ3CrVHgz6vGRt8687sH4oAA1qp8").explicitGet(),
        1900000,
        1526552510868L,
        None,
        100000,
        Base58.decode("4t2Xazb2SX").get,
        ByteStr.decodeBase58("eaV1i3hEiXyYQd6DQY7EnPg9XzpAvB9VA3bnpin2qJe4G36GZXaGnYKCgSf9xiQ61DcAwcBFzjSXh6FwCgazzFz").get
      )
      .right
      .get

    tx.json() shouldEqual js
  }

  property("negative") {
    for {
      (_, sender, recipient, amount, timestamp, _, feeAmount, attachment) <- transferParamGen
      sender                                                              <- accountGen
    } yield
      TransferTransactionV1.selfSigned(None, sender, recipient, amount, timestamp, None, feeAmount, attachment) should produce("insufficient fee")
  }
}
