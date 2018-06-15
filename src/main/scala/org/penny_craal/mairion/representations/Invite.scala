package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** An invite extended to an email address to join a [[Workspace]].
  * @param wsId The target workspace.
  * @param email The target user's email.
  * @param status This invite's [[Invite.Status]].
  * @param created The time when this invite was created.
  * @param expires The time when this invite expires.
  * @param groups A list of [[Group]]s into which the user will be added once they join.
  */
case class Invite(
  wsId: IdNumber,
  email: String,
  status: Invite.Status,
  created: ZonedDateTime,
  expires: Option[ZonedDateTime],
  groups: Seq[IdNumber],
) extends Resource.Plain

object Invite {
  /** Describes the status of this invite. */
  sealed trait Status extends Resource.Plain

  /** The invite has been made, but the recipient has not responded to it. */
  case object Pending extends Status

  /** The invite has been rejected by the recipient. */
  case object Rejected extends Status

  /** The invite has been accepted by the recipient. */
  case object Accepted extends Status
}

