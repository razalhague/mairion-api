package org.penny_craal.mairion.services

import cats.effect.IO
import org.http4s.implicits._
import org.http4s.{AuthedRequest, Method, Response, Status, Uri, Request => HttpRequest}
import org.penny_craal.mairion.representations.IdNumber
import org.scalatest.{FunSpec, Matchers}

class WorkspaceServiceSpec extends FunSpec with Matchers {
  def doRequest(method: Method, uriString: String, auth: Option[IdNumber], payload: String): Response[IO] = {
    val uri = Uri.fromString(uriString).right.get
    val r = for {
      request <- HttpRequest[IO](method, uri).withBody(payload)
      authReq = AuthedRequest[IO, Option[IdNumber]](auth, request)
      response <- WorkspaceService.service.orNotFound(authReq)
    } yield response
    r.unsafeRunSync()
  }

  describe("The WorkspaceService") {
    it("should handle unauthorized requests properly") {
      doRequest(Method.GET, "/", None, "").status should be (Status.Unauthorized)
    }
  }
}
