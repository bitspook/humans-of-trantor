package com.trantorinc.pms.standup

import java.time.Instant

import com.trantorinc.pms.infrastructure.Doobie._

class StandupModel {
  def findByType(standupType: String): ConnectionIO[Option[Standup]] = {
    findBy(fr"type = $standupType")
  }

  private def findBy(by: Fragment): ConnectionIO[Option[Standup]] = {
    (sql"SELECT ecode, project, standup, date, type FROM users WHERE " ++ by)
      .query[Standup]
      .option
  }
}

case class Standup(
    ecode: String,
    project: String,
    standup: String,
    date: Instant,
    standupType: String
) {}