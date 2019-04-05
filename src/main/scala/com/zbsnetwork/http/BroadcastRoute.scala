package com.zbsnetwork.http

import com.zbsnetwork.api.http.ApiError
import com.zbsnetwork.network._
import com.zbsnetwork.transaction.{Transaction, ValidationError}
import com.zbsnetwork.utx.UtxPool
import io.netty.channel.group.ChannelGroup

trait BroadcastRoute {
  def utx: UtxPool
  def allChannels: ChannelGroup

  protected def doBroadcast(v: Either[ValidationError, Transaction]): Either[ApiError, Transaction] = {
    val r = for {
      tx <- v
      r  <- utx.putIfNew(tx)
    } yield {
      val (added, _) = r
      if (added) allChannels.broadcastTx(tx, None)
      tx
    }

    r.left.map(ApiError.fromValidationError)
  }
}
