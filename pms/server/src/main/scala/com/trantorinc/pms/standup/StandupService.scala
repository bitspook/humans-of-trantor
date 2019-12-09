package com.trantorinc.pms.standup

import cats.implicits._
import com.trantorinc.pms.Fail
import com.trantorinc.pms.infrastructure.Doobie._
import com.typesafe.scalalogging.StrictLogging

class StandupService(
    standupModel: StandupModel
) extends StrictLogging {
  def findByType(standupType: String): ConnectionIO[Standup] = standupOrNotFound(standupModel.findByType(standupType))

  private def standupOrNotFound(op: ConnectionIO[Option[Standup]]): ConnectionIO[Standup] = {
    op.flatMap {
      case Some(user) => user.pure[ConnectionIO]
      case None       => Fail.NotFound("user").raiseError[ConnectionIO, Standup]
    }
  }
}