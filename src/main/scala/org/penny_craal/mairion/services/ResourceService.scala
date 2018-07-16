package org.penny_craal.mairion.services

import cats.effect.IO
import cats.~>
import org.http4s.AuthedService
import org.http4s.dsl.Http4sDsl
import org.penny_craal.mairion.representations.IdNumber
import org.penny_craal.mairion.resourcerepository.{FallibleIO, ResourceRepository}

/** A simple container for a service and the path it should be mounted to. */
abstract class ResourceService extends Http4sDsl[IO] {
  /** The path this service should be mounted to. */
  def path: String

  /** The service. */
  def service(compiler: ResourceRepository ~> FallibleIO): AuthedService[Option[IdNumber], IO]
}
