package org.penny_craal.mairion

import scala.concurrent.ExecutionContext.Implicits.global
import cats.effect.IO
import fs2.StreamApp
import org.http4s.server.blaze.BlazeBuilder
import org.penny_craal.mairion.services.{UserService, WorkspaceMembershipService, WorkspaceService}

/** The server application. */
object MairionServer extends StreamApp[IO] {
  /** List the services to be served. */
  private val services = List(
    UserService,
    WorkspaceService,
    WorkspaceMembershipService,
  )

  def stream(args: List[String], requestShutdown: IO[Unit]): fs2.Stream[IO, StreamApp.ExitCode] = {
    val interpreter = RrioSharedStateInterpreter
    val baseBlaze = BlazeBuilder[IO].bindHttp(8080, "0.0.0.0")
    val blazeWithServices = services.foldLeft(baseBlaze) { (blaze, unauthdService) =>
      val service = Authentication.middleware(interpreter)(unauthdService.service(interpreter))
      blaze.mountService(service, unauthdService.path)
    }
    blazeWithServices.serve
  }
}
