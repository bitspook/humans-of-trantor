package com.trantorinc.pms.standup

import com.trantorinc.pms.infrastructure.Doobie._
import com.typesafe.scalalogging.StrictLogging

class StandupService(
    standupModel: StandupModel
) extends StrictLogging {
  def find(ecode: Option[String], month: Option[Int]): ConnectionIO[List[Standup]] = standupModel.find(ecode, month)
}
