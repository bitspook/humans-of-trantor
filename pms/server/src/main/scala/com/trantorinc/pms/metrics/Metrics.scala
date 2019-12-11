package com.trantorinc.pms.metrics

import io.prometheus.client.hotspot

object Metrics {
  def init(): Unit = {
    hotspot.DefaultExports.initialize()
  }
}
