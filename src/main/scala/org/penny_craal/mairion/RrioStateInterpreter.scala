package org.penny_craal.mairion

import java.time.ZonedDateTime

import cats.data.{EitherT, State}
import cats.~>
import org.penny_craal.mairion.Request._
import org.penny_craal.mairion.resourcerepository.ResourceRepository.algebra._
import org.penny_craal.mairion.resourcerepository.{FallibleT, Filter, MairionError, RRResult, ResourceRepository}
import org.penny_craal.mairion.representations.{Delta, Group, IdNumber, OperationType, Permission, PermissionScope,
  Resource, ResourceType, User, WorkspaceMembership}
import org.penny_craal.mairion.representations.{ResourceType => RT}
import org.penny_craal.mairion.representations.PermissionScope._

/** A pure interpreter for turning [[resourcerepository.RRIO]] values into [[RrioStateInterpreter.FallibleRRState]]
  * values.
  */
object RrioStateInterpreter {
  /** The type value used as state by this interpreter. */
  type RRStateMap = Map[ResourceType, Map[IdNumber, Resource]]

  /** The state type used by this interpreter, without error handling. */
  type RRState[A] = State[RRStateMap, A]

  /** The state type used by this interpreter. */
  type FallibleRRState[A] = FallibleT[RRState, A]

  /** The types of resources this interpreter handles. */
  private val managedResourceTypes = List(
    RT.Workspace,
    RT.WorkspaceMembership,
    RT.User,
    RT.Group,
    RT.Permission,
    RT.Invite,
    RT.InviteStatus,
    RT.Transaction,
  )

  /** Creates a pure state interpreter.
    * @param time The current time.
    * @return The interpreter.
    */
  def at(time: ZonedDateTime): (ResourceRepository ~> FallibleRRState) = new (ResourceRepository ~> FallibleRRState) {
    override def apply[A](fa: ResourceRepository[A]): FallibleRRState[A] = {
      val resultFs: FallibleRRState[_] = fa match {
        case PerformRequest(request) => performRequest(request, time)
        case GetUserByName(name) => getUserByName(name)
        case GetHighestPermissionScope(userId, wsId, resourceType, operationType) =>
          getHighestPermissionScope(userId, wsId, resourceType, operationType)
        case GetJurisdictionWorkspaceId(request) => getJurisdictionWorkspaceId(request)
        case UsersShareWorkspace(userIdA, userIdB) => usersShareWorkspace(userIdA, userIdB)
        case operation => EitherT.leftT(MairionError.notImplemented(s"RrioStateInterpreter support for $operation"))
      }
      resultFs map (_.asInstanceOf[A])
    }
  }

  /** The initial state, a map of empty maps. */
  val initialState: RRStateMap = managedResourceTypes.map(_ -> Map[IdNumber, Resource]()).toMap

  /** Return true if both users have an active membership in a common workspace. */
  private def usersShareWorkspace(userIdA: IdNumber, userIdB: IdNumber): FallibleRRState[Boolean] =
    for {
      aWorkspaces <- activeWorkspacesForUser(userIdA)
      bWorkspaces <- activeWorkspacesForUser(userIdB)
    } yield (aWorkspaces intersect bWorkspaces).nonEmpty

  /** Returns a set of IDs of all the workspaces for which the user has an active membership. */
  private def activeWorkspacesForUser(userId: IdNumber): FallibleRRState[Set[IdNumber]] = for {
    memberships <- getResourceMap[WorkspaceMembership](RT.WorkspaceMembership)
    activeMemberships = memberships.values collect {
      case WorkspaceMembership(wsId, `userId`, _, _, None) => wsId
    }
  } yield activeMemberships.toSet

  /** Updates state so that the given resource is with the ID. */
  private def setResource(
    resourceType: ResourceType,
    resId: IdNumber,
    res: Resource
  ): FallibleRRState[RRResult[Resource]] =
    EitherT.right(State(s => s.updated(resourceType, s(resourceType).updated(resId, res)) -> Map(resId -> res)))

  /** Retrieves the specified resource from the state. */
  private def getResource[R <: Resource](resourceType: ResourceType, resId: IdNumber): FallibleRRState[R] = {
    val res: FallibleRRState[Resource] =
      EitherT(State.inspect(s => s(resourceType).get(resId) toRight MairionError.resourceNotFound(resourceType, resId)))
    res map (_.asInstanceOf[R])
  }

  /** Retrieves all resources of the given type from the state. */
  private def getResourceMap[R <: Resource](resourceType: ResourceType): FallibleRRState[Map[IdNumber, R]] =
    EitherT.right(State.inspect(s => s(resourceType) mapValues (_.asInstanceOf[R])))

  /** Removes the specified resource from the state. */
  private def deleteResource(resourceType: ResourceType, resId: IdNumber): FallibleRRState[RRResult[Resource]] = {
    val deleted = State.modify((s: RRStateMap) => s.updated(resourceType, s(resourceType) - resId))
    EitherT.right(deleted map (_ => Map.empty))
  }

  /** See [[ResourceRepository.getJurisdictionWorkspaceId]]. */
  private def getJurisdictionWorkspaceId(request: Request[_]): FallibleRRState[IdNumber] = {
    if (!request.resourceType.isInstanceOf[ResourceType.Discretionary])
      EitherT.leftT(MairionError.noJurisdiction("non-discretionary resources"))
    else request match {
      case Browse(RT.Workspace, Seq(Filter.ById(RT.User, _))) =>
        EitherT.leftT(MairionError.noJurisdiction("browsing workspaces"))
      case Read(RT.Workspace, wsId) => EitherT.rightT(wsId)
      case Edit(RT.Workspace, wsId, _) => EitherT.rightT(wsId)
      case Add(RT.Workspace, _) =>
        EitherT.leftT(MairionError.noJurisdiction("adding a workspace"))
      case Delete(RT.Workspace, wsId) => EitherT.rightT(wsId)
      case Browse(RT.WorkspaceMembership, Seq(Filter.ById(RT.Workspace, wsId))) => EitherT.rightT(wsId)
      case Read(RT.WorkspaceMembership, membershipId) => for {
        membership <- getResource[WorkspaceMembership](RT.WorkspaceMembership, membershipId)
      } yield membership.wsId
      case Edit(RT.WorkspaceMembership, membershipId, _) => for {
        membership <- getResource[WorkspaceMembership](RT.WorkspaceMembership, membershipId)
      } yield membership.wsId
      case Add(RT.WorkspaceMembership, res) => EitherT.rightT(res.asInstanceOf[WorkspaceMembership].wsId)
      case Delete(RT.WorkspaceMembership, _) =>
        EitherT.leftT(MairionError.internal("workspace memberships cannot be deleted"))
      case Add(RT.WorkspaceDismissal, (wsId: IdNumber, _)) =>
        EitherT.rightT(wsId)
      case _ if request.resourceType == RT.WorkspaceDismissal =>
        EitherT.leftT(MairionError.internal(s"the only acceptable operation on ${request.resourceType} is Add"))
      case Browse(RT.Group, Seq(Filter.ById(RT.Workspace, wsId))) => EitherT.rightT(wsId)
      case Read(RT.Group, gId) => for {
        group <- getResource[Group](RT.Group, gId)
      } yield group.wsId
      case Edit(RT.Group, gId, _) => for {
        group <- getResource[Group](RT.Group, gId)
      } yield group.wsId
      case Add(RT.Group, group: Group) => EitherT.rightT(group.wsId)
      case Delete(RT.Group, gId) => for {
        group <- getResource[Group](RT.Group, gId)
      } yield group.wsId
      case Browse(RT.Permission, Seq(Filter.ById(RT.Group, gId))) => for {
        group <- getResource[Group](RT.Group, gId)
      } yield group.wsId
      case Add(RT.Permission, perm: Permission) => for {
        group <- getResource[Group](RT.Group, perm.groupId)
      } yield group.wsId
      case Delete(RT.Permission, pId) => for {
        perm <- getResource[Permission](RT.Permission, pId)
        group <- getResource[Group](RT.Group, perm.groupId)
      } yield group.wsId
      case Browse(RT.Invite, Seq(Filter.ById(RT.Workspace, wsId))) => EitherT.rightT(wsId)
      case other =>
        EitherT.leftT(MairionError.notImplemented(s"getJurisdictionWorkspaceId($other)"))
    }
  }

  /** See [[ResourceRepository.getHighestPermissionScope]]. */
  private def getHighestPermissionScope(
    userId: IdNumber,
    wsId: IdNumber,
    resourceType: ResourceType.Discretionary,
    operationType: OperationType
  ): FallibleRRState[Option[PermissionScope]] = for {
    groupIds <- getGroupIdsForUserInWorkspace(userId, wsId)
    permissions <- getResourceMap[Permission](RT.Permission)
    scopes = permissions.values collect {
      case Permission(groupId, `operationType`, scope, `resourceType`) if groupIds contains groupId => scope
    }
  } yield scopes.foldLeft(None: Option[PermissionScope])(morePermissiveScope)

  /** Returns all the groups of which the given user is a member of in the given workspace. */
  private def getGroupIdsForUserInWorkspace(userId: IdNumber, wsId: IdNumber): FallibleRRState[Seq[IdNumber]] = for {
    groups <- getResourceMap[Group](RT.Group)
    groupIds = groups collect {
      case (id, Group(_, `wsId`, members, _)) if members.contains(userId) => id
    }
  } yield groupIds.toSeq

  /** Returns the more permissive scope of the two. */
  private def morePermissiveScope(
    optScopeA: Option[PermissionScope],
    scopeB: PermissionScope
  ): Option[PermissionScope] = (optScopeA, scopeB) match {
    case (None, b) => Some(b)
    case (Some(SameWorkspace), _)
       | (_, SameWorkspace)
      => Some(SameWorkspace)
    case _ => Some(SameUser)
  }

  /** See [[ResourceRepository.getUserByName]]. */
  private def getUserByName[A](name: String): FallibleRRState[(IdNumber, User)] = for {
    users <- getResourceMap[User](RT.User)
    user <- EitherT.fromEither[RRState](users find (_._2.name == name) toRight MairionError.notAuthenticated)
  } yield user

  /** See [[ResourceRepository.performRequest]]. */
  private def performRequest(request: Request[_], time: ZonedDateTime): FallibleRRState[RRResult[Resource]] =
    request match {
      case Browse(resourceType, _) =>
        getResourceMap(resourceType)
      case Read(resourceType, id) =>
        getResource[Resource](resourceType, id) map (res => Map(id -> res))
      case Edit(RT.User, userId, delta) => for {
        user <- getResource[User](RT.User, userId)
        modifiedUser <- applyDelta(delta, user)
        result <- setResource(RT.User, userId, modifiedUser)
      } yield result
      // either a resignation or a dismissal, we need to set the exit time here
      case Edit(RT.WorkspaceMembership, membershipId, delta) => for {
        membership <- getResource[WorkspaceMembership](RT.WorkspaceMembership, membershipId)
        modifiedMembership <- applyDelta(delta, membership)
        detailsWithTime = modifiedMembership.termination.map(_.withTime(Some(time)))
        modifiedMembershipWithTime = modifiedMembership.copy(termination = detailsWithTime)
        result <- setResource(RT.WorkspaceMembership, membershipId, modifiedMembershipWithTime)
      } yield result
      case Edit(resourceType, resId, delta) => for {
        res <- getResource[Resource](resourceType, resId)
        modifiedRes <- applyDelta(delta, res)
        result <- setResource(resourceType, resId, modifiedRes)
      } yield result
      case Add(RT.WorkspaceResignation, _)
         | Add(RT.WorkspaceDismissal, _)
        => EitherT.leftT(MairionError.internal("StateCompiler does not handle Resignations and Dismissals"))
      case Add(RT.User, user: User) => for {
        _ <- emailIsUnique(user.email)
        resId <- newId(RT.User)
        result <- setResource(RT.User, resId, user)
      } yield result
      case add @ Add(RT.WorkspaceMembership, wsMembership @ WorkspaceMembership(_, _, _, None, _)) =>
        val wsMembershipWithTime = wsMembership.copy(joined = Some(time))
        val addWithTimedMembership = add.copy(resource = wsMembershipWithTime)
        performRequest(addWithTimedMembership, time)
      case Add(resourceType, res) => for {
        resId <- newId(resourceType)
        result <- setResource(resourceType, resId, res.asInstanceOf[Resource])
      } yield result
      case Delete(resourceType, resId) =>
        deleteResource(resourceType, resId)
    }

  /** Returns a new, unused IdNumber for the given resource type. */
  private def newId(resourceType: ResourceType): FallibleRRState[IdNumber] =
    getResourceMap[Resource](resourceType) subflatMap { resources =>
      val longIds = resources.keys map (_.value)
      val newLongId = longIds.foldLeft(IdNumber.minimum.value - 1)(scala.math.max) + 1
      IdNumber.unapply(newLongId) toRight MairionError.internal(s"could not make IdNumber from $newLongId")
    }

  /** Returns Unit if the given email is not in use, otherwise an error. */
  private def emailIsUnique(email: String): FallibleRRState[Unit] =
    getResourceMap[User](RT.User) subflatMap (users =>
      if (users.values exists (_.email == email))
        Left(MairionError.duplicateEmail(email))
      else
        Right(())
    )

  /** Returns an updated resource by applying the delta function. */
  private def applyDelta[R <: Resource](delta: Delta[_], resource: R): FallibleRRState[R] = {
    val realDelta = delta.asInstanceOf[Delta[R]]
    EitherT.fromEither(realDelta(resource))
  }
}
