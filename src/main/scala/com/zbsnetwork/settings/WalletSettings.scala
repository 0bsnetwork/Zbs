package com.zbsnetwork.settings

import java.io.File

import com.zbsnetwork.common.state.ByteStr

case class WalletSettings(file: Option[File], password: Option[String], seed: Option[ByteStr])
