package com.zbsplatform

import com.zbsplatform.settings.WalletSettings
import com.zbsplatform.wallet.Wallet

trait TestWallet {
  protected val testWallet = {
    val wallet = Wallet(WalletSettings(None, "123", None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
