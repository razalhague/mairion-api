package org.penny_craal.mairion
package services

import cats.data.EitherT
import cats.effect.IO
import org.http4s.AuthedService
import org.penny_craal.mairion.dslextensions._
import org.penny_craal.mairion.Request._
import org.penny_craal.mairion.resourcerepository._
import org.penny_craal.mairion.representations.{Delta, IdNumber, User, ResourceType => RT}
import org.penny_craal.mairion.representations.json.implicits._

/** A [[ResourceService]] that handles requests that return a [[representations.User]]. */
object UserService extends ResourceService {
  val parseHttpRequest: HttpRequestParser[User] = {
    case GET -> Root :? WorkspaceQueryParam(wsId) +& GroupQueryParam(groupId) as Some(_) =>
      EitherT.rightT(Browse[User](RT.User, Seq(Filter.ById(RT.Workspace, wsId), Filter.ById(RT.Group, groupId))))
    case GET -> Root :? WorkspaceQueryParam(wsId) as Some(_) =>
      EitherT.rightT(Browse(RT.User, Seq(Filter.ById(RT.Workspace, wsId))))
    case (req @ POST -> Root) as None =>
      EitherT.right(req.as[User.In] map (Add(RT.User, _)))
    case GET -> Root / ResourceIdVar(id) as Some(_) =>
      EitherT.rightT(Read(RT.User, id))
    case GET -> Root / "self" as Some(userId) =>
      EitherT.rightT(Read(RT.User, userId))
    case (req @ PATCH -> Root / "self") as Some(userId) =>
      EitherT.right(req.as[Delta[User]] map (Edit(RT.User, userId, _)))
    case DELETE -> Root / "self" as Some(userId) =>
      EitherT.rightT(Delete(RT.User, userId))

    case GET -> Root as Some(_) => EitherT.leftT(MairionError.missingParam("workspace"))
    case (GET -> Root :? WorkspaceQueryParam(_) +& GroupQueryParam(_) as None)
       | (GET -> Root :? WorkspaceQueryParam(_) as None)
       | (GET -> Root / ResourceIdVar(_) as None)
       | (GET -> Root / "self" as None)
       | (PATCH -> Root / "self" as None)
       | (DELETE -> Root / "self" as None)
      => EitherT.leftT(MairionError.notAuthenticated)
    case POST -> Root as Some(_) => EitherT.leftT(MairionError.authenticated)
  }

  val hashToUser: User.In => User = {
    case User.In(name, password, email) => User(name, Authentication.hashPassword(password), email)
  }


  /** Hashes the password within the request. */
  val preProcessor: PartialFunction[Identified[Request[User]], Identified[Request[User]]] = {
    case Identified(id, Add(RT.User, userIn: User.In)) => Identified(id, Add(RT.User, hashToUser(userIn)))
    case other => other
  }

  override val path = "/users"

  override val service: AuthedService[Option[IdNumber], IO] =
    makeResourceService[User](parseHttpRequest, preProcessor andThen defaultProcessor[User])
}
