package com.zbsplatform.state.diffs.smart.predef

import com.zbsplatform.lagonaki.mocks.TestBlock
import com.zbsplatform.lang.v1.compiler.CompilerV1
import com.zbsplatform.lang.v1.parser.Parser
import com.zbsplatform.settings.TestFunctionalitySettings
import com.zbsplatform.state._
import com.zbsplatform.state.diffs.{ENOUGH_AMT, assertDiffAndState}
import com.zbsplatform.transaction.smart.SetScriptTransaction
import com.zbsplatform.transaction.smart.script.v1.ScriptV1
import com.zbsplatform.transaction.{GenesisTransaction, PaymentTransaction}
import com.zbsplatform.utils.dummyCompilerContext
import com.zbsplatform.{NoShrink, TransactionGen}
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ObsoleteTransactionBindingsTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink {

  def script(g: GenesisTransaction, p: PaymentTransaction): String =
    s"""
      | let genTx = extract(transactionById(base58'${g.id().base58}'))
      | let payTx = extract(transactionById(base58'${p.id().base58}'))
      |
      | let genTotal = match genTx {
      |   case gen: GenesisTransaction =>
      |     let genId = gen.id == base58'${g.id().base58}'
      |     let genFee = gen.fee == ${g.assetFee._2}
      |     let genTimestamp = gen.timestamp== ${g.timestamp}
      |     let genVersion = gen.version == 1
      |     let genAmount = gen.amount == ${g.amount}
      |     let genRecipient = gen.recipient == Address(base58'${g.recipient.address}')
      |     genId && genFee && genTimestamp && genVersion && genAmount && genRecipient
      |    case _ => false
      |  }
      |
      | let payTotal = match payTx {
      |   case pay: PaymentTransaction =>
      |     let payId = pay.id == base58'${p.id().base58}'
      |     let payFee = pay.fee == ${p.assetFee._2}
      |     let payTimestamp = pay.timestamp== ${p.timestamp}
      |     let payVersion = pay.version == 1
      |     let payAmount = pay.amount == ${p.amount}
      |     let payRecipient = pay.recipient == Address(base58'${p.recipient.address}')
      |
      |     let bodyBytes = pay.bodyBytes == base64'${ByteStr(p.bodyBytes.apply()).base64}'
      |     let sender = pay.sender == addressFromPublicKey(base58'${ByteStr(p.sender.publicKey).base58}')
      |     let senderPublicKey = pay.senderPublicKey == base58'${ByteStr(p.sender.publicKey).base58}'
      |     let signature = pay.proofs[0]== base58'${p.signature.base58}'
      |     let empty1 = pay.proofs[1]== base58''
      |     let empty2 = pay.proofs[2]== base58''
      |     let empty3 = pay.proofs[3]== base58''
      |     let empty4 = pay.proofs[4]== base58''
      |     let empty5 = pay.proofs[5]== base58''
      |     let empty6 = pay.proofs[6]== base58''
      |     let empty7 = pay.proofs[7]== base58''
      |
      |     let payBindings = payId && payFee && payTimestamp && payVersion && payAmount && payRecipient
      |     let payBindings1 = bodyBytes && sender && senderPublicKey && signature
      |     let payBindings2 = empty1 && empty2 && empty3 && empty4 && empty5 && empty6 && empty7
      |
      |     payBindings && payBindings1 && payBindings2
      |   case _ => false
      | }
      |
      | genTotal && payTotal
      |
    """.stripMargin

  val preconditionsAndPayments = for {
    master    <- accountGen
    recipient <- otherAccountGen(candidate = master)
    ts        <- positiveIntGen
    fee       <- smallFeeGen
    genesis: GenesisTransaction = GenesisTransaction.create(master, ENOUGH_AMT * 3, ts).explicitGet()
    payment                     = PaymentTransaction.create(master, recipient, ENOUGH_AMT * 2, fee, ts).explicitGet()
    untypedScript               = Parser(script(genesis, payment)).get.value
    typedScript                 = ScriptV1(CompilerV1(dummyCompilerContext, untypedScript).explicitGet()._1).explicitGet()
    setScriptTransaction: SetScriptTransaction = SetScriptTransaction
      .selfSigned(1, recipient, Some(typedScript), 100000000L, ts)
      .explicitGet()
    nextTransfer <- transferGeneratorPV2(ts, recipient, master.toAddress, ENOUGH_AMT)
  } yield (genesis, payment, setScriptTransaction, nextTransfer)

  val settings = TestFunctionalitySettings.Enabled.copy(blockVersion3AfterHeight = 100)
  property("Diff doesn't break invariant before block version 3") {
    forAll(preconditionsAndPayments) {
      case ((genesis, payment, setScriptTransaction, nextTransfer)) =>
        assertDiffAndState(Seq(TestBlock.create(Seq(genesis, payment, setScriptTransaction))), TestBlock.create(Seq(nextTransfer)), settings) {
          (blockDiff, newState) =>
            ()
        }
    }
  }
}
