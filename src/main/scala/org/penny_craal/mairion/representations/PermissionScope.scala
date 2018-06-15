package org.penny_craal.mairion.representations

/** Describes the scope of a permission. */
sealed trait PermissionScope

object PermissionScope {
  /** The permission applies to resources which are owned by the same [[User]] as the one who is making the request. */
  case object SameUser extends PermissionScope

  /** The permission applies to any resource within the same [[Workspace]] */
  case object SameWorkspace extends PermissionScope
}
