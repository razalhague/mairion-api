package org.penny_craal.mairion.representations

/** Describes a permission to perform an operation on a type of resource with a specified scope.
  * @param groupId The ID of the group that has this permission.
  * @param operationType The type of operation permitted.
  * @param scope The scope of this permission.
  * @param resourceType The type of resource this permission applies to.
  */
case class Permission(
  groupId: IdNumber,
  operationType: OperationType,
  scope: PermissionScope,
  resourceType: ResourceType.Discretionary,
) extends Resource.Plain

