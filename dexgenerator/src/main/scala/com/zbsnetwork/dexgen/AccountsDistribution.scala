package com.zbsplatform.dexgen

import cats.Show
import com.zbsplatform.dexgen.AccountsDistribution.Settings

class AccountsDistribution(settings: Settings, valid: Int, invalid: Int, fake: Int) {}

object AccountsDistribution {

  case class Settings(valid: Int, invalid: Int, fake: Int)

  object Settings {
    implicit val toPrintable: Show[Settings] = { x =>
      import x._

      s"""Number of addresses:
         |valid orders: $valid
         |invalid orders: $invalid
         |fake orders: $fake""".stripMargin

    }
  }

}
