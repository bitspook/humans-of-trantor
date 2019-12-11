package com.trantorinc.pms.employee

import cats.data.NonEmptyList
import com.trantorinc.pms.http.Http
import com.trantorinc.pms.infrastructure.Doobie._
import com.trantorinc.pms.infrastructure.Json._
import com.trantorinc.pms.util.ServerEndpoints
import doobie.util.transactor.Transactor
import monix.eval.Task
import sttp.tapir._

class EmployeeApi(http: Http, employeeService: EmployeeService, xa: Transactor[Task]) {
  import EmployeeApi._
  import http._

  private val EmployeePath = "employees"

  private val getEmployeeEndpoint = baseEndpoint.get
    .in(EmployeePath.and(project))
    .out(jsonBody[GetEmployee_OUT])
    .serverLogic { project =>
      (for {
        employee <- employeeService.find(project).transact(xa)
      } yield GetEmployee_OUT(data = employee)).toOut
    }

  val endpoints: ServerEndpoints =
    NonEmptyList
      .of(
        getEmployeeEndpoint
      )
      .map(_.tag("employees"))
}

object EmployeeApi {
  val project = query[Option[String]]("project")

  case class GetEmployee_OUT(data: List[Employee])
}
