package com.trantorinc.pms.config

case class Sensitive(value: String) extends AnyVal {
  override def toString: String = "***"
}
