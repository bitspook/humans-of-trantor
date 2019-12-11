package com.trantorinc.pms.employee

import com.trantorinc.pms.http.Http
import com.trantorinc.pms.util.BaseModule
import doobie.util.transactor.Transactor
import monix.eval.Task

trait EmployeeModule extends BaseModule {
  lazy val employeeModel = new EmployeeModel
  lazy val employeeApi = new EmployeeApi(http, employeeService, xa)
  lazy val employeeService = new EmployeeService(employeeModel)

  def http: Http
  def xa: Transactor[Task]
}
