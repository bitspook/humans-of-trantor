package com.trantorinc.pms

import java.nio.ByteBuffer

import cats.effect.Resource
import com.trantorinc.pms.config.ConfigModule
import com.trantorinc.pms.infrastructure.DB
import monix.eval.Task
import monix.reactive.Observable
import sttp.client.SttpBackend
import sttp.client.asynchttpclient.WebSocketHandler
import sttp.client.asynchttpclient.monix.AsyncHttpClientMonixBackend

/**
  * Initialised resources needed by the application to start.
  */
trait InitModule extends ConfigModule {
  lazy val db: DB = new DB(config.db)
  lazy val baseSttpBackend: Resource[Task, SttpBackend[Task, Observable[ByteBuffer], WebSocketHandler]] =
    AsyncHttpClientMonixBackend.resource()
}
