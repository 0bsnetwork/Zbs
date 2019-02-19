package com.zbsnetwork.it

import com.typesafe.config.Config

trait Nodes {
  protected def nodes: Seq[Node]
  protected def nodeConfigs: Seq[Config]
}
