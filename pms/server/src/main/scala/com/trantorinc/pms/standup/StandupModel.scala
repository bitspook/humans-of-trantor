package com.trantorinc.pms.standup

import java.time.Instant

import com.trantorinc.pms.infrastructure.Doobie._

class StandupModel {
  val eventName = "RECEIVED_STANDUP_UPDATE";
  val eventVersion = "v1";

  def find(ecode: Option[String]): ConnectionIO[List[Standup]] = {
    ecode match {
      case Some(ecode) => findBy(fr"payload->>'ecode' = $ecode")
      case None => findBy(fr"1 = 1")
    }
  }

  private def findBy(by: Fragment): ConnectionIO[List[Standup]] = {
    // Unique standup for type and date.
    // TODO: Convert this query to a view
    (sql"""
     |SELECT DISTINCT ON (payload->>'date', payload->>'type')
     |  payload->>'ecode' as ecode,
     |  payload->>'project' as project,
     |  payload->>'standup' as standup,
     |  payload->>'date' as date,
     |  payload->>'type' as ecode
     |FROM (
     |  SELECT payload from store.store
     |  WHERE name = $eventName
     |    AND version = $eventVersion
     |  ORDER BY created_at DESC
     |) AS store
     |WHERE
     """ ++ by)
      .stripMargin
      .query[Standup]
      .to[List]
  }
}

case class Standup(
    ecode: String,
    project: String,
    standup: String,
    date: Instant,
    standupType: String
) {}
