package com.zbsnetwork

import org.scalacheck.Shrink

trait NoShrink {
  implicit def noShrink[A]: Shrink[A] = Shrink(_ => Stream.empty)
}
