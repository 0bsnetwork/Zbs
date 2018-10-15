package com.zbsplatform.settings

import java.io.File

import com.zbsplatform.state.ByteStr

case class WalletSettings(file: Option[File], password: String, seed: Option[ByteStr])
