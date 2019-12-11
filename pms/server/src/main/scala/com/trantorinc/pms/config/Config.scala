package com.trantorinc.pms.config

import com.trantorinc.pms.http.HttpConfig
import com.trantorinc.pms.infrastructure.DBConfig

/**
  * Maps to the `application.conf` file. Configuration for all modules of the application.
  */
case class Config(db: DBConfig, api: HttpConfig)
