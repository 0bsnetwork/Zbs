package com.zbsplatform.lang

import com.zbsplatform.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.zbsplatform.lang.Global // Hack for IDEA
}
