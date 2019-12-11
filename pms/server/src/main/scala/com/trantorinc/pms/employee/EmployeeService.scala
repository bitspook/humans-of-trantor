package com.trantorinc.pms.employee

import com.trantorinc.pms.infrastructure.Doobie._
import com.typesafe.scalalogging.StrictLogging

class EmployeeService(
    employeeModel: EmployeeModel
) extends StrictLogging {
  def find(project: Option[String]): ConnectionIO[List[Employee]] = employeeModel.find(project)
}
