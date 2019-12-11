package com.trantorinc.pms.util

import java.time.Clock

import com.trantorinc.pms.config.Config

trait BaseModule {
  def idGenerator: IdGenerator
  def clock: Clock
  def config: Config
}
