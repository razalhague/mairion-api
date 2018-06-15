package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Describes a [[User]]'s membership in a [[Workspace]].
  * @param wsId The workspace the user is/was a member of.
  * @param userId The ID of the user.
  * @param isWorkspaceOwner Is/was the user an owner of the workspace.
  * @param joined The time when the user joined the workspace.
  * @param termination Details concerning the termination of the user's membership, None if still a member.
  */
case class WorkspaceMembership(
  wsId: IdNumber,
  userId: IdNumber,
  isWorkspaceOwner: Boolean,
  joined: Option[ZonedDateTime],
  termination: Option[Termination],
) extends Resource.Plain
