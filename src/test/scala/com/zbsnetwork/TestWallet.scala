package com.zbsnetwork

import com.zbsnetwork.settings.WalletSettings
import com.zbsnetwork.wallet.Wallet

trait TestWallet {
  protected val testWallet: Wallet = {
    val wallet = Wallet(WalletSettings(None, Some("123"), None))
    wallet.generateNewAccounts(10)
    wallet
  }
}
