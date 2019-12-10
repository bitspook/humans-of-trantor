package com.trantorinc.pms.standup

import cats.data.NonEmptyList
import com.trantorinc.pms.http.Http
import com.trantorinc.pms.infrastructure.Doobie._
import com.trantorinc.pms.infrastructure.Json._
import com.trantorinc.pms.util.ServerEndpoints
import doobie.util.transactor.Transactor
import monix.eval.Task
import sttp.tapir.{EndpointInput, query}

class StandupApi(http: Http, standupService: StandupService, xa: Transactor[Task]) {
  import StandupApi._
  import http._

  private val StandupPath = "standup"

  private val getStandupEndpoint = baseEndpoint.get
    .in(StandupPath)
    .in(ecode)
    .out(jsonBody[GetStandup_OUT])
    .serverLogic { (ecode) =>
      (for {
        standup <- standupService.find(ecode).transact(xa)
      } yield GetStandup_OUT(data = standup)).toOut
    }

  val endpoints: ServerEndpoints =
    NonEmptyList
      .of(
        getStandupEndpoint
      )
      .map(_.tag("standup"))
}

object StandupApi {
  val ecode: EndpointInput[Option[String]] = query[Option[String]]("ecode")

  case class GetStandup_IN(ecode: Option[String])
  case class GetStandup_OUT(data: List[Standup])
}
