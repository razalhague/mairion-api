package org.penny_craal.mairion
package services

import cats.data.{EitherT, Kleisli}
import cats.effect.IO
import cats.~>
import org.http4s.AuthedService
import org.penny_craal.mairion.dslextensions._
import org.penny_craal.mairion.Request._
import org.penny_craal.mairion.resourcerepository.{Fallible, FallibleIO, Filter, MairionError, ResourceRepository => RR}
import org.penny_craal.mairion.representations.Termination.In
import org.penny_craal.mairion.representations.{IdNumber, Termination, WorkspaceMembership, ResourceType => RT}
import org.penny_craal.mairion.representations.json.implicits._

/** A [[ResourceService]] that handles requests that return a
  * [[representations.WorkspaceMembership]].
  */
object WorkspaceMembershipService extends ResourceService {
  val parseHttpRequest: HttpRequestParser[WorkspaceMembership] = {
    case (GET -> Root / ResourceIdVar(_) / "memberships" as None)
       | (POST -> Root / ResourceIdVar(_) / "memberships" / "resignations" as None)
       | (POST -> Root / ResourceIdVar(_) / "memberships" / "dismissals" as None)
      => EitherT.leftT(MairionError.notAuthenticated)

    case GET -> Root / ResourceIdVar(wsId) / "memberships" as Some(_) =>
      EitherT.rightT(Browse[WorkspaceMembership](RT.WorkspaceMembership, Seq(Filter.ById(RT.Workspace, wsId))))
    case (req @ POST -> Root / ResourceIdVar(wsId) / "memberships" / "resignations") as Some(_) =>
      EitherT.right(req.as[In.Resignation] map (r => Add[WorkspaceMembership](RT.WorkspaceResignation, (wsId, r))))
    case (req @ POST -> Root / ResourceIdVar(wsId) / "memberships" / "dismissals") as Some(_) =>
      EitherT.right(req.as[In.Dismissal] map (d => Add[WorkspaceMembership](RT.WorkspaceDismissal, (wsId, d))))
  }

  /** Creates an [[ApiRequestProcessor]] for the specified workspace membership termination.
    * @param resourceType The type of workspace membership termination.
    * @param userIdFromIdRequest A function to extract the ID of the user who made the request.
    * @return An [[ApiRequestProcessor]].
    */
  private def makeProcessor(
    resourceType: RT,
    userIdFromIdRequest: Identified[Request.Add[_]] => IdNumber
  ): ApiRequestProcessor[WorkspaceMembership] = {
    case idRequest @ Identified(Some(requester), Add(`resourceType`, (wsId: IdNumber, termination: Termination.In))) =>
      val userId = userIdFromIdRequest(idRequest.asInstanceOf[Identified[Request.Add[_]]])
      val delta = Kleisli[Fallible, WorkspaceMembership, WorkspaceMembership] {
        wsMembership => Right(wsMembership.copy(termination = Some(termination.full(requester))))
      }
      for {
        usersMemberships <- RR.browse[WorkspaceMembership](RT.WorkspaceMembership, Filter.ById(RT.User, userId))
        membershipId = usersMemberships.collectFirst { case (id, WorkspaceMembership(`wsId`, _, _, _, None)) => id }.get
        edited <- RR.edit(RT.WorkspaceMembership, membershipId, delta)
      } yield edited
  }

  private def getUserIdFromIdRequestForDismissed(idRequest: Identified[Request.Add[_]]): IdNumber =
    idRequest.request.resource match { case (_, dismissed: In.Dismissal) => dismissed.dismissee }

  val processor: ApiRequestProcessor[WorkspaceMembership] = Seq(
    makeProcessor(RT.WorkspaceResignation,  _.userId.get),
    makeProcessor(RT.WorkspaceDismissal, getUserIdFromIdRequestForDismissed),
  ) reduce (_ orElse _)

  override val path = "/workspaces"

  override def service(compiler: RR ~> FallibleIO): AuthedService[Option[IdNumber], IO] =
    makeResourceService[WorkspaceMembership](parseHttpRequest, compiler, processor)
}
