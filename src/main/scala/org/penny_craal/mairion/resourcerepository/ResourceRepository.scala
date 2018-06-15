package org.penny_craal.mairion.resourcerepository

import cats.free.Free
import cats.free.Free.liftF
import org.penny_craal.mairion.Request
import org.penny_craal.mairion.Request._
import org.penny_craal.mairion.representations.{Delta, IdNumber, OperationType, PermissionScope, Resource, ResourceType,
  User}

/** The parent trait of objects describing the algebra of the ResourceRepositoty free monad.
  * @tparam A What type of value does this operation return.
  */
trait ResourceRepository[A]

/** Companion object that defines the algebra for the free monad, and functions for creating monadic values. Error
  * handling is defined by interpreters of the free monad.
  */
object ResourceRepository {
  /** Case classes and objects that define the ResourceRepository algebra. */
  object algebra {
    /** See [[ResourceRepository.performRequest]]. */
    case class PerformRequest[R <: Resource](request: Request[R]) extends ResourceRepository[RRResult[R]]

    /** See [[ResourceRepository.getUserByName]]. */
    case class GetUserByName(name: String) extends ResourceRepository[(IdNumber, User)]

    /** See [[ResourceRepository.getHighestPermissionScope]]. */
    case class GetHighestPermissionScope(
      userId: IdNumber,
      wsId: IdNumber,
      resourceType: ResourceType.Discretionary,
      operationType: OperationType
    ) extends ResourceRepository[Option[PermissionScope]]

    /** See [[ResourceRepository.getJurisdictionWorkspaceId]]. */
    case class GetJurisdictionWorkspaceId[R <: Resource](request: Request[R]) extends ResourceRepository[IdNumber]

    /** See [[ResourceRepository.usersShareWorkspace]]. */
    case class UsersShareWorkspace(userId1: IdNumber, userId2: IdNumber) extends ResourceRepository[Boolean]
  }

  import org.penny_craal.mairion.resourcerepository.ResourceRepository.algebra._

  /** Returns a map of IdNumber to resource, filtered as requested.
    * @param resourceType The type of resources being browsed.
    * @param filters A list of [[Filter]]s.
    * @tparam R The type of resource being browsed.
    * @return A [[RRResult]] containing 0 or more resources.
    */
  def browse[R <: Resource](resourceType: ResourceType, filters: Filter*): RRIO[RRResult[R]] =
    performRequest(Browse(resourceType, filters))

  /** Returns a map of IdNumber to resource, containing a single result.
    * @param resourceType The type of resource being read.
    * @param resourceId The ID of the resource.
    * @tparam R The Type of resource being read.
    * @return A [[RRResult]] containing one resource.
    */
  def read[R <: Resource](resourceType: ResourceType, resourceId: IdNumber): RRIO[RRResult[R]] =
    performRequest(Read(resourceType, resourceId))

  /** Updates a resource and returns a map of IdNumber to resource, containing the updated resource.
    * @param resourceType The type of resource being edited.
    * @param resourceId The ID of the resource.
    * @param delta A function that returns an updated resource.
    * @tparam R The type of resource being edited.
    * @return A [[RRResult]] containing the updated resource.
    */
  def edit[R <: Resource](resourceType: ResourceType, resourceId: IdNumber, delta: Delta[R]): RRIO[RRResult[R]] =
    performRequest(Edit(resourceType, resourceId, delta))

  /** Adds a resource and returns a map of IdNumber to resource, containing the added resource.
    * @param resourceType The type of resource being added.
    * @param resource The resource to be added.
    * @tparam R The type of resource being added.
    * @return A [[RRResult]] containing the added resource.
    */
  def add[R <: Resource](resourceType: ResourceType, resource: Resource): RRIO[RRResult[R]] =
    performRequest(Add(resourceType, resource))

  /** Deletes a resource.
    * @param resourceType The type of resource being deleted.
    * @param resourceId The ID of the resource to be deleted.
    * @tparam R The type of resource being deleted.
    * @return An empty [[RRResult]].
    */
  def delete[R <: Resource](resourceType: ResourceType, resourceId: IdNumber): RRIO[RRResult[R]] =
    performRequest(Delete(resourceType, resourceId))

  /** Returns the result of the request.
    * @param request Request to be performed.
    * @tparam R Type of resource given as a result.
    */
  def performRequest[R <: Resource](request: Request[R]): RRIO[RRResult[R]] =
    liftF[ResourceRepository, RRResult[R]](PerformRequest(request))

  /** Finds the user's ID and details based on the name.
    * @param name The user's name.
    */
  def getUserByName(name: String): RRIO[(IdNumber, User)] =
    liftF[ResourceRepository, (IdNumber, User)](GetUserByName(name))

  /** Get the highest permission scope for the given parameters.
    * @param userId ID of the user.
    * @param wsId ID of the workspace.
    * @param resourceType The type of resource the user is attempting to operate upon.
    * @param operationType The type of operation the user is attempting to perform.
    * @return Highest scope of all the relevant permissions, or None if no relevant permissions.
    */
  def getHighestPermissionScope(
    userId: IdNumber,
    wsId: IdNumber,
    resourceType: ResourceType.Discretionary,
    operationType: OperationType
  ): RRIO[Option[PermissionScope]] =
    liftF[ResourceRepository, Option[PermissionScope]](GetHighestPermissionScope(
      userId,
      wsId,
      resourceType,
      operationType
    ))

  /** Returns the ID of the workspace that has jurisdiction over the given request.
    * @param request The request whose jurisdiction workspace is to be determined.
    * @tparam R The type of request.
    */
  def getJurisdictionWorkspaceId[R <: Resource](request: Request[R]): RRIO[IdNumber] =
    liftF[ResourceRepository, IdNumber](GetJurisdictionWorkspaceId(request))

  /** Returns whether the two user IDs share a workspace.
    * @param userIdA The first user ID.
    * @param userIdB The second user ID.
    * @return True if both users are members of the same workspace, otherwise false.
    */
  def usersShareWorkspace(userIdA: IdNumber, userIdB: IdNumber): RRIO[Boolean] =
    liftF[ResourceRepository, Boolean](UsersShareWorkspace(userIdA, userIdB))

  /** Lifts a pure value into the free monad.
    * @param a The value to be lifted.
    * @tparam A The type of value to be lifted.
    * @return The value in a monad.
    */
  def pure[A](a: A): RRIO[A] = Free.pure(a)
}
