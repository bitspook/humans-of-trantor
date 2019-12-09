package com.trantorinc.pms.standup

import cats.data.NonEmptyList
import com.trantorinc.pms.http.Http
import com.trantorinc.pms.infrastructure.Doobie._
import com.trantorinc.pms.infrastructure.Json._
import com.trantorinc.pms.util.ServerEndpoints
import doobie.util.transactor.Transactor
import monix.eval.Task

class StandupApi(http: Http, standupService: StandupService, xa: Transactor[Task]) {
  import StandupApi._
  import http._

  private val StandupPath = "standup"

  private val getStandupEndpoint = baseEndpoint.get
    .in(StandupPath)
    .out(jsonBody[GetStandup_OUT])
    .serverLogic { _ =>
      (for {
        standup <- standupService.findByType("1").transact(xa)
      } yield GetStandup_OUT(standup = standup.standup)).toOut
    }

  val endpoints: ServerEndpoints =
    NonEmptyList
      .of(
        getStandupEndpoint
      )
      .map(_.tag("standup"))
}

object StandupApi {
  case class GetStandup_OUT(standup: String)
}
