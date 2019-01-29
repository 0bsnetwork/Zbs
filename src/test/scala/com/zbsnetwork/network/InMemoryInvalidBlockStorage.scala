package com.zbsnetwork.network

import com.zbsnetwork.common.state.ByteStr
import com.zbsnetwork.transaction.ValidationError
import com.zbsnetwork.transaction.ValidationError.GenericError

class InMemoryInvalidBlockStorage extends InvalidBlockStorage {

  var s: Set[ByteStr] = Set.empty[ByteStr]

  override def add(blockId: ByteStr, validationError: ValidationError): Unit = s += blockId

  override def find(blockId: ByteStr): Option[ValidationError] = {
    if (s.contains(blockId)) Some(GenericError("Unknown")) else None
  }

}
