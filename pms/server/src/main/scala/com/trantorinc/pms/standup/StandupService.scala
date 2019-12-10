package com.trantorinc.pms.standup

import com.trantorinc.pms.infrastructure.Doobie._
import com.typesafe.scalalogging.StrictLogging

class StandupService(
    standupModel: StandupModel
) extends StrictLogging {
  def find(ecode: Option[String]): ConnectionIO[List[Standup]] = standupModel.find(ecode)
}