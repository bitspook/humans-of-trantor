package com.trantorinc.pms.employee

import com.trantorinc.pms.infrastructure.Doobie._

class EmployeeModel {
  val eventName = "RECEIVED_EMPLOYEE_UPDATE";
  val eventVersion = "v1";

  def find(project: Option[String]): ConnectionIO[List[Employee]] = {
    var where = fr""

    where = project match {
      case Some(project) => where ++ fr"AND payload->>'project'=$project"
      case None          => where
    }

    findBy(where)
  }

  private def findBy(by: Fragment): ConnectionIO[List[Employee]] = {
    // TODO: Convert this query to a view
    (sql"""
         |SELECT DISTINCT on (payload->>'ecode')
         |  payload->>'ecode' as ecode,
         |  payload->>'name' as name,
         |  payload->>'email' as email,
         |  payload->>'project' as project,
         |  payload->>'designation' as designation
         |FROM store.store
         |WHERE name='DISCOVERED_EMPLOYEE'
         """ ++ by).stripMargin
      .query[Employee]
      .to[List]
  }
}

case class Employee(
    ecode: String,
    name: String,
    email: String,
    project: Option[String],
    designation: Option[String]
) {}
