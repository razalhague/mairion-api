package org.penny_craal.mairion

import cats.data.EitherT
import org.penny_craal.mairion.resourcerepository.Credentials.BasicAuth
import org.penny_craal.mairion.resourcerepository.{Credentials, FallibleT, Filter, MairionError, RRIO,
  ResourceRepository}
import org.penny_craal.mairion.representations.{Group, IdNumber, Invite, Permission, PermissionScope, Resource,
  ResourceType, Transaction, User, WorkspaceMembership}
import org.penny_craal.mairion.representations.{ResourceType => RT}
import org.penny_craal.mairion.Request._

/** Authentication and authorization functions that operate over [[resourcerepository.RRIO]]. */
object ResourceRepositoryAuth {
  /** Tests if the credentials match any user in the repository and returns their user ID if they match.
    * @param credentials The credentials to be tested.
    * @return Some(ID) if authentication successful, None if not.
    */
  def getUserIdIfCredentialsAuthenticate(credentials: Credentials): FallibleT[RRIO, Option[IdNumber]] =
    credentials match {
      case BasicAuth(userName, password) => EitherT.right {
        for {
          pair <- ResourceRepository.getUserByName(userName)
          (id, user) = pair
          hashedPassword = Authentication.hashPassword(password)
        } yield if (user.hashedPassword == hashedPassword) Some(id) else None
      }
      case _ => EitherT.leftT(MairionError.internal("unsupported authentication scheme"))
    }

  /** Returns the specified resource, or an error.
    * @param resourceType The type of resource to get.
    * @param resourceId The ID of the resource to get.
    * @tparam R The type of resource to get.
    * @return The specified resource, or an error.
    */
  def getResource[R <: Resource](resourceType: ResourceType, resourceId: IdNumber): FallibleT[RRIO, R] = EitherT {
    for (resource <- ResourceRepository.read[R](resourceType, resourceId))
    yield resource.values.headOption toRight MairionError.internal(s"could not find $resourceType # $resourceId")
  }

  /** Returns whether the given user owns the specified resource.
    * @param userId The user whose ownership is in question.
    * @param resourceType The type of resource being inspected.
    * @param resourceId The ID of the resource being inspected.
    * @return True if the user owns the resource, false if they don't, an error if something goes wrong (for example if
    *         the resource doesn't exist).
    */
  def userOwnsResource(userId: IdNumber, resourceType: ResourceType, resourceId: IdNumber): FallibleT[RRIO, Boolean] = {
    resourceType match {
      case RT.WorkspaceMembership | RT.WorkspaceDismissal =>
        getResource[WorkspaceMembership](RT.WorkspaceMembership, resourceId) map (_.userId == userId)
      case RT.Invite => for {
        user <- getResource[User](RT.User, userId)
        invite <- getResource[Invite](RT.Invite, resourceId)
      } yield user.email == invite.email
      case RT.Transaction => getResource[Transaction](RT.Transaction, resourceId) map (_.authorId == userId)
      case RT.Group => getResource[Group](RT.Group, resourceId) map (_.owners contains userId)
      case RT.Permission => for {
        permission <- getResource[Permission](RT.Permission, resourceId)
        isOwner <- userOwnsResource(userId, RT.Group, permission.groupId)
      } yield isOwner
      case RT.Workspace =>
        getActiveMembership(userId, resourceId) map {
          case Some(membership: WorkspaceMembership) => membership.isWorkspaceOwner
          case _ => false
        }
      case _ => EitherT.leftT(MairionError.notImplemented(s"userOwnsResource($resourceType)"))
    }
  }

  /** Returns whether the request is authorized.
    * @tparam R The type of result the request would have.
    * @return True if authorized, false if not, error if something unexpected happened.
    */
  def isRequestAuthorized[R <: Resource]: Identified[Request[R]] => FallibleT[RRIO, Boolean] =
    Seq(
      isNonDiscretionaryRequestAuthorized[R],
      isDiscretionaryRequestAuthorized[R],
      isUnauthenticatedRequestAuthorized[R],
      authorizationNotImplemented[R],
    ) reduce (_ orElse _)

  /** Terminal case that returns a notImplemented error value. */
  private def authorizationNotImplemented[R <: Resource]
  : PartialFunction[Identified[Request[R]], FallibleT[RRIO, Boolean]] = {
    case idRequest => EitherT.leftT(MairionError.notImplemented(s"isRequestAuthorized($idRequest)"))
  }

  /** Handles cases where the user did not authenticate themselves. */
  private def isUnauthenticatedRequestAuthorized[R <: Resource]
  : PartialFunction[Identified[Request[R]], FallibleT[RRIO, Boolean]] = {
    case Identified(None, Add(RT.User, _)) => EitherT.rightT(true)
    case Identified(None, _) => EitherT.rightT(false)
  }

  /** Handles cases with static rules. */
  private def isNonDiscretionaryRequestAuthorized[R <: Resource]
  : PartialFunction[Identified[Request[R]], FallibleT[RRIO, Boolean]] = {
    case Identified(Some(_), Add(RT.Workspace, _)) => EitherT.rightT(true)
    case Identified(Some(requesterId), Browse(RT.Workspace, filters)) =>
      EitherT.rightT(filters contains Filter.ById(RT.User, requesterId))
    case idRequest @ Identified(Some(requesterId), Browse(RT.Invite, filters)) =>
      getResource[User](RT.User, requesterId) subflatMap { user =>
        if (filters contains Filter.ByEmail(user.email)) Right(true) // browse own invites
        else Left(MairionError.notImplemented(s"requestIsAuthorized($idRequest)"))
      }
    case Identified(Some(requesterId), Edit(RT.User, targetUserId, _)) => EitherT.rightT(requesterId == targetUserId)
    case Identified(Some(requesterId), Delete(RT.User, targetUserId)) => EitherT.rightT(requesterId == targetUserId)
    case Identified(Some(requesterId), Read(RT.User, targetUserId)) =>
      if (requesterId == targetUserId) EitherT.rightT(true)
      else EitherT.right(ResourceRepository.usersShareWorkspace(requesterId, targetUserId))
    case Identified(Some(requesterId), Read(RT.Invite, invId)) => userOwnsResource(requesterId, RT.Invite, invId)
    case Identified(Some(requesterId), Edit(RT.InviteStatus, invId, _)) =>
      userOwnsResource(requesterId, RT.Invite, invId)
    case Identified(Some(requesterId), Add(RT.WorkspaceResignation, (wsId: IdNumber, _))) =>
      isUserInWorkspace(requesterId, wsId)
    case idRequest @ Identified(Some(requesterId), Browse(RT.User, filters)) =>
      filters collectFirst {
        case Filter.ById(RT.Workspace, wsId) => isUserInWorkspace(requesterId, wsId)
      } getOrElse EitherT.leftT(MairionError.internal("invalid request: " + idRequest.toString))
  }

  /** Handles requests that depend on the permission system. */
  private def isDiscretionaryRequestAuthorized[R <: Resource]
  : PartialFunction[Identified[Request[R]], FallibleT[RRIO, Boolean]] = {
    case Identified(Some(userId), request) if request.resourceType.isInstanceOf[ResourceType.Discretionary] =>
      val discretionary = request.resourceType.asInstanceOf[ResourceType.Discretionary]
      val parameters = for {
        wsId <- ResourceRepository.getJurisdictionWorkspaceId(request)
        maxScope <- ResourceRepository.getHighestPermissionScope(userId, wsId, discretionary, request.operationType)
      } yield (request, userId, wsId, maxScope)
      EitherT.right(parameters) flatMap (isDiscretionaryRequestAuthorizedByScope[R] _).tupled
  }

  /** Handles the logic of deciding whether the request is permitted by the scope. */
  private def isDiscretionaryRequestAuthorizedByScope[R <: Resource](
    request: Request[R],
    userId: IdNumber,
    wsId: IdNumber,
    maxScopeOption: Option[PermissionScope]
  ): FallibleT[RRIO, Boolean] = {
    (maxScopeOption, request) match {
      case (None, _) => EitherT.rightT(false)
      case (Some(_), Add(_, _)) => EitherT.rightT(true) // only possible to add owned items, scope is irrelevant
      case (Some(PermissionScope.SameWorkspace), _) => isUserInWorkspace(userId, wsId)
      case (Some(PermissionScope.SameUser), irRequest: Request.OnIndividualResource[R]) =>
        userOwnsResource(userId, request.resourceType, irRequest.resourceId)
      case (Some(PermissionScope.SameUser), Browse(_, _)) =>
        EitherT.leftT(MairionError.notImplemented("authorization of browsing only owned items"))
      case other => EitherT.leftT(MairionError.notImplemented(s"isAuthorized($other)"))
    }
  }

  /** Returns a user's active membership in a workspace, if applicable.
    * @param userId The user being inspected.
    * @param wsId The workspace being inspected.
    * @return Some(membership) if the user is currently a member of the workspace, None if not.
    */
  def getActiveMembership(userId: IdNumber, wsId: IdNumber): FallibleT[RRIO, Option[WorkspaceMembership]] =
    EitherT.right(
      for {
        memberships <- ResourceRepository.browse[WorkspaceMembership](
          RT.WorkspaceMembership,
          Filter.ById(RT.Workspace, wsId),
          Filter.ById(RT.User, userId)
        )
      } yield memberships.values.find {
        case membership if membership.termination.isEmpty => true
        case _ => false
      }
    )

  /** Returns whether the user is currently a member of the workspace.
    * @param userId The user being inspected.
    * @param wsId The workspace being inspected.
    * @return True if the user is currently a member, false otherwise.
    */
  def isUserInWorkspace(userId: IdNumber, wsId: IdNumber): FallibleT[RRIO, Boolean] =
    getActiveMembership(userId, wsId) map (_.nonEmpty)
}
