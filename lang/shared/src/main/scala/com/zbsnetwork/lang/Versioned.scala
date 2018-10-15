package com.zbsplatform.lang

trait Versioned {
  type V <: ScriptVersion
  val version: V
}
