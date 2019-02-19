package com.zbsnetwork.lang

import com.zbsnetwork.lang.v1.BaseGlobal

package object hacks {
  private[lang] val Global: BaseGlobal = com.zbsnetwork.lang.Global // Hack for IDEA
}
