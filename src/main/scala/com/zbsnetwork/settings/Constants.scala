package com.zbsplatform.settings

import com.zbsplatform.Version
import com.zbsplatform.utils.ScorexLogging

/**
  * System constants here.
  */
object Constants extends ScorexLogging {
  val ApplicationName = "zbs"
  val AgentName       = s"Zbs v${Version.VersionString}"

  val UnitsInZbs = 100000000L
  val TotalZbs   = 100000000L
}
