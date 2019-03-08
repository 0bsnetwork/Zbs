package com.zbsnetwork.settings

import com.zbsnetwork.Version
import com.zbsnetwork.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "zbs"
  val AgentName       = s"Zbs v${Version.VersionString}"

  val UnitsInZbs = 100000000L
  val TotalZbs   = 51000000L
}
