package com.zbsnetwork.state.diffs

import com.zbsnetwork.account.{Address, AddressScheme, PrivateKeyAccount}
import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.common.utils.EitherExt2
import com.zbsnetwork.features.BlockchainFeatures
import com.zbsnetwork.lagonaki.mocks.TestBlock
import com.zbsnetwork.lang.StdLibVersion
import com.zbsnetwork.lang.contract.Contract
import com.zbsnetwork.lang.contract.Contract.{CallableAnnotation, CallableFunction}
import com.zbsnetwork.lang.v1.FunctionHeader
import com.zbsnetwork.lang.v1.FunctionHeader.{Native, User}
import com.zbsnetwork.lang.v1.compiler.Terms
import com.zbsnetwork.lang.v1.compiler.Terms._
import com.zbsnetwork.lang.v1.evaluator.ctx.impl.zbs.FieldNames
import com.zbsnetwork.settings.TestFunctionalitySettings
import com.zbsnetwork.state._
import com.zbsnetwork.transaction.{AssetId, GenesisTransaction, ValidationError}
import com.zbsnetwork.transaction.smart.script.ContractScript
import com.zbsnetwork.transaction.smart.{ContractInvocationTransaction, SetScriptTransaction}
import com.zbsnetwork.transaction.smart.ContractInvocationTransaction.Payment
import com.zbsnetwork.transaction.assets.IssueTransactionV2
import com.zbsnetwork.transaction.smart.script.v1.ExprScript
import com.zbsnetwork.transaction.transfer.TransferTransactionV2
import com.zbsnetwork.{NoShrink, TransactionGen, WithDB}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ContractInvocationTransactionDiffTest extends PropSpec with PropertyChecks with Matchers with TransactionGen with NoShrink with WithDB {

  private val fs = TestFunctionalitySettings.Enabled.copy(
    preActivatedFeatures =
      Map(BlockchainFeatures.SmartAccounts.id -> 0, BlockchainFeatures.SmartAssets.id -> 0, BlockchainFeatures.Ride4DApps.id -> 0))

  val assetAllowed = ExprScript(TRUE).explicitGet()
  val assetBanned  = ExprScript(FALSE).explicitGet()

  def dataContract(senderBinding: String, argName: String, funcName: String) = Contract(
    List.empty,
    List(
      CallableFunction(
        CallableAnnotation(senderBinding),
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User(FieldNames.WriteSet),
            List(FUNCTION_CALL(
              Native(1102),
              List(
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("argument"), REF(argName))),
                FUNCTION_CALL(User("DataEntry"), List(CONST_STRING("sender"), GETTER(GETTER(REF(senderBinding), "caller"), "bytes")))
              )
            ))
          )
        )
      )),
    None
  )

  def paymentContract(senderBinding: String,
                      argName: String,
                      funcName: String,
                      recipientAddress: Address,
                      recipientAmount: Long,
                      assetId: Option[AssetId] = None) = Contract(
    List.empty,
    List(
      CallableFunction(
        CallableAnnotation(senderBinding),
        Terms.FUNC(
          funcName,
          List(argName),
          FUNCTION_CALL(
            User(FieldNames.TransferSet),
            List(FUNCTION_CALL(
              Native(1102),
              List(
                FUNCTION_CALL(
                  User(FieldNames.ContractTransfer),
                  List(
                    FUNCTION_CALL(User("Address"), List(CONST_BYTESTR(recipientAddress.bytes))),
                    CONST_LONG(recipientAmount),
                    assetId.fold(REF("unit"): EXPR)(id => CONST_BYTESTR(id))
                  )
                )
              )
            ))
          )
        )
      )),
    None
  )

  def dataContractGen(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield dataContract(senderBinging, argBinding, func)

  def paymentContractGen(address: Address, amount: Long, assetId: Option[AssetId] = None)(func: String) =
    for {
      senderBinging <- validAliasStringGen
      argBinding    <- validAliasStringGen
    } yield paymentContract(senderBinging, argBinding, func, address, amount, assetId)

  def preconditionsAndSetContract(
      senderBindingToContract: String => Gen[Contract],
      invokerGen: Gen[PrivateKeyAccount] = accountGen,
      masterGen: Gen[PrivateKeyAccount] = accountGen,
      payment: Option[Payment] = None): Gen[(List[GenesisTransaction], SetScriptTransaction, ContractInvocationTransaction)] =
    for {
      master  <- masterGen
      invoker <- invokerGen
      ts      <- timestampGen
      genesis: GenesisTransaction  = GenesisTransaction.create(master, ENOUGH_AMT, ts).explicitGet()
      genesis2: GenesisTransaction = GenesisTransaction.create(invoker, ENOUGH_AMT, ts).explicitGet()
      fee         <- smallFeeGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- validAliasStringGen
      contract    <- senderBindingToContract(funcBinding)
      script      = ContractScript(StdLibVersion.V3, contract)
      setContract = SetScriptTransaction.selfSigned(master, script.toOption, fee, ts).explicitGet()
      fc          = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg))))
      ci          = ContractInvocationTransaction.selfSigned(invoker, master, fc, payment, fee, ts).explicitGet()
    } yield (List(genesis, genesis2), setContract, ci)

  property("invoking contract results contract's state") {
    forAll(for {
      r <- preconditionsAndSetContract(dataContractGen)
    } yield (r._1, r._2, r._3)) {
      case (genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.accountData(genesis(0).recipient) shouldBe AccountDataInfo(
              Map(
                "sender"   -> BinaryDataEntry("sender", ci.sender.toAddress.bytes),
                "argument" -> BinaryDataEntry("argument", ci.fc.args(0).asInstanceOf[CONST_BYTESTR].bs)
              ))
        }
    }
  }

  property("invoking payment contract results in accounts state") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      r <- preconditionsAndSetContract(contractGen)
    } yield (a, am, r._1, r._2, r._3)) {
      case (acc, amount, genesis, setScript, ci) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, None) shouldBe amount
        }
    }
  }

  val chainId   = AddressScheme.current.chainId
  val enoughFee = CommonValidation.ScriptExtraFee + CommonValidation.FeeConstants(IssueTransactionV2.typeId) * CommonValidation.FeeUnit

  property("invoking contract recive payment") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      r <- preconditionsAndSetContract(contractGen, invokerGen = Gen.oneOf(Seq(invoker)), payment = Some(Payment(1, Some(asset.id()))))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(acc, None) shouldBe amount
            newState.balance(invoker, Some(asset.id())) shouldBe (asset.quantity - 1)
            newState.balance(ci.contractAddress, Some(asset.id())) shouldBe 1
        }
    }
  }

  property("asset script ban invoking contract with payment") {
    forAll(for {
      a  <- accountGen
      am <- smallFeeGen
      contractGen = (paymentContractGen(a, am) _)
      invoker <- accountGen
      ts      <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, invoker, "Asset#1".getBytes, "".getBytes, 1000000, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      r <- preconditionsAndSetContract(contractGen, invokerGen = Gen.oneOf(Seq(invoker)), payment = Some(Payment(1, Some(asset.id()))))
    } yield (a, am, r._1, r._2, r._3, asset, invoker)) {
      case (acc, amount, genesis, setScript, ci, asset, invoker) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("TransactionNotAllowedByScript")
        }
    }
  }

  property("invoking contract make payment by asset") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffAndState(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) {
          case (blockDiff, newState) =>
            newState.balance(master, Some(asset.id())) shouldBe (asset.quantity - amount)
            newState.balance(acc, Some(asset.id())) shouldBe amount
        }
    }
  }

  property("invoking contract disable by payment smart asset") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetBanned), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, am, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)))
    } yield (a, am, r._1, r._2, r._3, asset, master)) {
      case (acc, amount, genesis, setScript, ci, asset, master) =>
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("TransactionNotAllowedByScript")
        }
    }
  }

  property("Contract payment should be positive") {
    forAll(for {
      a <- accountGen
      quantity = 1000000
      am     <- Gen.choose[Long](1L, quantity)
      master <- accountGen
      ts     <- timestampGen
      asset = IssueTransactionV2
        .selfSigned(chainId, master, "Asset#1".getBytes, "".getBytes, quantity, 8, false, Some(assetAllowed), enoughFee, ts)
        .explicitGet()
      contractGen = (paymentContractGen(a, -1, Some(asset.id())) _)
      r <- preconditionsAndSetContract(contractGen, masterGen = Gen.oneOf(Seq(master)))
    } yield (a, am, r._1, r._2, r._3, asset, master, ts)) {
      case (acc, amount, genesis, setScript, ci, asset, master, ts) =>
        val t = TransferTransactionV2.selfSigned(Some(asset.id()), master, acc, asset.quantity / 10, ts, None, enoughFee, Array[Byte]()).explicitGet()
        assertDiffEi(Seq(TestBlock.create(genesis ++ Seq(asset, t, setScript))), TestBlock.create(Seq(ci)), fs) { blockDiffEi =>
          blockDiffEi should produce("NegativeAmount")
        }
    }
  }

  property("payment should be positive") {
    forAll(for {
      invoker     <- accountGen
      master      <- accountGen
      ts          <- timestampGen
      arg         <- genBoundedString(1, 32)
      funcBinding <- validAliasStringGen
      fc = Terms.FUNCTION_CALL(FunctionHeader.User(funcBinding), List(CONST_BYTESTR(ByteStr(arg))))
      ci = ContractInvocationTransaction.selfSigned(invoker, master, fc, Some(Payment(-1, None)), enoughFee, ts)
    } yield (ci)) { ci =>
      ci shouldBe 'left
      ci.left.get.isInstanceOf[ValidationError.NegativeAmount] should be(true)

    }
  }

}
