package org.phenoscape.kb.server

import cats.effect.Blocker
import org.http4s.HttpRoutes._
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.blaze.server.BlazeServerBuilder
import zio._
import zio.blocking.{Blocking, blockingExecutor}
import zio.interop.catz._
import zio.interop.catz.implicits._

object Server extends App {

  private val dsl = Http4sDsl[Task]

  import dsl._

  override def run(args: List[String]): ZIO[ZEnv, Nothing, ExitCode] = server.exitCode

  val server: RIO[Blocking, Unit] =
    ZIO.runtime[Any].flatMap { implicit runtime =>
      for {
        routes <- getRoutes
        serve <- BlazeServerBuilder[Task](runtime.platform.executor.asEC)
          .bindHttp(8080)
          .withHttpApp(Router("/" -> routes).orNotFound)
          .serve
          .compile
          .drain
      } yield serve
    }

  def getBlocker: RIO[Blocking, Blocker] =
    for {
      executor <- blockingExecutor
      blocker = Blocker.liftExecutionContext(executor.asEC)
    } yield blocker

  def getRoutes: ZIO[Blocking, Throwable, HttpRoutes[Task]] =
    for {
      blocker <- getBlocker
    } yield of[Task] {
      case request @ GET -> Root / "js" / path if List(".js", ".css", ".map", ".html", ".webm").exists(path.endsWith)  =>
        StaticFile.fromResource[Task]("/js/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ HEAD -> Root / "js" / path if List(".js", ".css", ".map", ".html", ".webm").exists(path.endsWith) =>
        StaticFile.fromResource[Task]("/js/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ GET -> Root / "css" / path if List(".css").exists(path.endsWith)  =>
        StaticFile.fromResource[Task]("/css/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ HEAD -> Root / "css" / path if List(".css").exists(path.endsWith) =>
        StaticFile.fromResource[Task]("/css/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ GET -> Root / "img" / path if List(".gif", ".png").exists(path.endsWith)  =>
        StaticFile.fromResource[Task]("/img/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ HEAD -> Root / "img" / path if List(".gif", ".png").exists(path.endsWith) =>
        StaticFile.fromResource[Task]("/img/" + path, blocker, Some(request))
          .getOrElseF(NotFound())
      case request @ GET -> path                                                                               =>
        println(path)
        StaticFile.fromResource[Task]("/index.html", blocker, Some(request))
          .getOrElseF(NotFound())
    }

}
