package com.zbsplatform.transaction

trait VersionedTransaction {
  def version: Byte
}
