package com.zbsnetwork.settings

import java.io.File

import com.zbsnetwork.common.state.ByteStr
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.{ArbitraryTypeReader, ValueReader}

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])

object WalletSettings {
  implicit val walletSettingsValueReader: ValueReader[WalletSettings] = ArbitraryTypeReader.arbitraryTypeValueReader
}
