package com.zbsnetwork.wallet

import com.zbsnetwork.utils._

object PasswordProvider extends ScorexLogging {

  def askPassword(): String = {

    Option(System.console()) match {
      case None =>
        log.error("CANNOT GET CONSOLE TO ASK WALLET PASSWORD")
        log.error(
          "Probably, it happens because you trying to start Zbs node using supervisor service (like systemd) without specified wallet password.")
        forceStopApplication(PasswordNotSpecified)
        ""

      case Some(console) =>
        console
          .writer()
          .write("Enter password for your wallet\n")

        console
          .readPassword("Password > ")
          .mkString

    }
  }

}
