package com.trantorinc.pms.standup

import com.trantorinc.pms.http.Http
import com.trantorinc.pms.util.BaseModule
import doobie.util.transactor.Transactor
import monix.eval.Task

trait StandupModule extends BaseModule {
  lazy val standupModel = new StandupModel
  lazy val standupApi = new StandupApi(http, standupService, xa)
  lazy val standupService = new StandupService(standupModel)

  def http: Http
  def xa: Transactor[Task]
}
