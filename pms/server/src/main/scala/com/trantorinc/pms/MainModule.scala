package com.trantorinc.pms

import java.time.Clock

import cats.data.NonEmptyList
import com.trantorinc.pms.http.{Http, HttpApi}
import com.trantorinc.pms.infrastructure.InfrastructureModule
import com.trantorinc.pms.metrics.MetricsModule
import com.trantorinc.pms.standup.StandupModule
import com.trantorinc.pms.util.{DefaultIdGenerator, IdGenerator, ServerEndpoints}
import monix.eval.Task

/**
  * Main application module. Depends on resources initialised in [[InitModule]].
  */
trait MainModule
    extends StandupModule
    with MetricsModule
    with InfrastructureModule {

  override lazy val idGenerator: IdGenerator = DefaultIdGenerator
  override lazy val clock: Clock = Clock.systemUTC()

  lazy val http: Http = new Http()

  private lazy val endpoints: ServerEndpoints = standupApi.endpoints
  private lazy val adminEndpoints: ServerEndpoints = NonEmptyList.of(metricsApi.metricsEndpoint, versionApi.versionEndpoint)

  lazy val httpApi: HttpApi = new HttpApi(http, endpoints, adminEndpoints, collectorRegistry, config.api)

  lazy val startBackgroundProcesses: Task[Unit] = Task {}
}
