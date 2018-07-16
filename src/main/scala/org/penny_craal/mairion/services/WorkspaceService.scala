package org.penny_craal.mairion
package services

import cats.data.EitherT
import cats.effect.IO
import cats.~>
import org.http4s.AuthedService
import org.penny_craal.mairion.dslextensions._
import org.penny_craal.mairion.Request._
import org.penny_craal.mairion.resourcerepository._
import org.penny_craal.mairion.representations.{Delta, Group, IdNumber, Permission, PermissionScope, Workspace, WorkspaceMembership, OperationType => OT, ResourceType => RT}
import org.penny_craal.mairion.representations.json.implicits._

/** A [[ResourceService]] that handles requests that return a [[representations.Workspace]]. */
object WorkspaceService extends ResourceService {
  val parseHttpRequest: HttpRequestParser[Workspace] = {
    case GET -> Root as Some(userId) =>
      EitherT.rightT(Browse(RT.Workspace, Seq(Filter.ById(RT.User, userId))))
    case (req @ POST -> Root) as Some(_) =>
      EitherT.right(req.as[Workspace] map (Add(RT.Workspace, _)))
    case GET -> Root / ResourceIdVar(id) as Some(_) =>
      EitherT.rightT(Read(RT.Workspace, id))
    case (req @ PATCH -> Root / ResourceIdVar(id)) as Some(_) =>
      EitherT.right(req.as[Delta[Workspace]] map (Edit(RT.Workspace, id, _)))
    case DELETE -> Root / ResourceIdVar(id) as Some(_) =>
      EitherT.rightT(Delete(RT.Workspace, id))

    case (GET -> Root as None)
       | (POST -> Root as None)
       | (GET -> Root / ResourceIdVar(_) as None)
       | (PATCH -> Root / ResourceIdVar(_) as None)
       | (DELETE -> Root / ResourceIdVar(_) as None)
      => EitherT.leftT(MairionError.notAuthenticated)
  }

  val processor: ApiRequestProcessor[Workspace] = {
    case Identified(Some(creatorId), add @ Add(RT.Workspace, _: Workspace)) => for {
      wsMap <- ResourceRepository.performRequest(add)
      (wsId, ws) = wsMap.head
      membership = WorkspaceMembership(wsId, creatorId, isWorkspaceOwner = true, None, None)
      _ <- ResourceRepository.add[WorkspaceMembership](RT.WorkspaceMembership, membership)
      gMap <- ResourceRepository.add[Group](RT.Group, defaultAdminGroup(wsId, creatorId))
      (gId, _) = gMap.head
      _ <- addPermissions(defaultAdminGroupPermissions(gId))
    } yield Map(wsId -> ws)
  }

  private def defaultAdminGroup(wsId: IdNumber, userId: IdNumber): Group =
    Group("administrators", wsId, Seq(userId), Seq(userId))
  private val defaultAdminOperationTypes = Seq(OT.Browse, OT.Read, OT.Edit, OT.Add, OT.Delete)
  private val defaultAdminResourceTypes = Seq(RT.Workspace, RT.WorkspaceMembership, RT.WorkspaceDismissal, RT.Group,
    RT.Permission, RT.Invite, RT.Transaction)
  private def defaultAdminGroupPermissions(gId: IdNumber): Seq[Permission] = for {
    operationType <- defaultAdminOperationTypes
    resourceType <- defaultAdminResourceTypes
  } yield Permission(gId, operationType, PermissionScope.SameWorkspace, resourceType)

  private def addPermissions(permissions: Seq[Permission]): RRIO[RRResult[Permission]] = {
    val noResults = ResourceRepository.pure(Map.empty[IdNumber, Permission])
    permissions.foldLeft(noResults)((accumulator, permission) => for {
      resultsSoFar <- accumulator
      nextResult <- ResourceRepository.add[Permission](RT.Permission, permission)
    } yield resultsSoFar ++ nextResult)
  }

  override val path = "/workspaces"

  override def service(compiler: ResourceRepository ~> FallibleIO): AuthedService[Option[IdNumber], IO] =
    makeResourceService[Workspace](parseHttpRequest, compiler, processor)
}
