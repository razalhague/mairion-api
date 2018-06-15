package org.penny_craal.mairion.representations

/** Describes the type of a resource. */
sealed trait ResourceType

object ResourceType {
  /** Describes resource types that are handled by the permission system. Generally only contains resources that are
    * associated with a specific workspace and make sense to be restrictable by the administrators of that workspace.
    */
  sealed trait Discretionary extends ResourceType

  /** Describes resource types that may be received or transmitted through the API. */
  sealed trait Plain extends ResourceType

  /** Describes resource types for which transaction history is maintained. */
  sealed trait Transactable extends ResourceType

  case object Workspace extends Plain with Discretionary
  case object WorkspaceMembership extends Plain with Discretionary
  case object WorkspaceResignation extends Plain
  case object WorkspaceDismissal extends Plain with Discretionary
  case object User extends Plain
  case object Group extends Plain with Discretionary
  case object Permission extends Plain with Discretionary
  case object Invite extends Plain with Discretionary
  case object InviteStatus extends Plain
  case object Transaction extends Plain with Discretionary

  case object Task extends Transactable
  case object WorkEntry extends Transactable
  case object Period extends Transactable
  case object Goal extends Transactable
  case object Comment extends Transactable
}
