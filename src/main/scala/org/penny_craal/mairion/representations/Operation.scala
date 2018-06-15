package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Describes an operation that is part of a [[Transaction]]. */
sealed trait Operation {
  /** The time this operation was performed. */
  val performed: ZonedDateTime

  /** The type of operation this is. */
  val operationType: OperationType.Transactable

  /** The type of resource this operation is on. */
  val resourceType: ResourceType

  /** The ID of the resource this operation targets, if any. */
  val resourceIdOpt: Option[IdNumber]
}

object Operation {
  /** Describes an operation that adds a resource.
    * @param performed See [[Operation.performed]].
    * @param resourceType See [[Operation.resourceType]].
    * @param resource The resource added in this operation.
    * @tparam R The type of resource added in this operation.
    */
  case class Add[R <: Resource.Transactable](
    performed: ZonedDateTime,
    resourceType: ResourceType,
    resource: R
  ) extends Operation {
    val operationType: OperationType.Add.type = OperationType.Add
    override val resourceIdOpt: None.type = None
  }

  /** Describes an operation that edits a resource.
    * @param performed See [[Operation.performed]].
    * @param resourceType See [[Operation.resourceType]].
    * @param resourceId See [[Operation.resourceIdOpt]].
    * @param delta A [[Delta]] that returns a modified resource.
    * @tparam R The type of resource modified by this operation.
    */
  case class Edit[R <: Resource.Transactable](
    performed: ZonedDateTime,
    resourceType: ResourceType,
    resourceId: IdNumber,
    delta: Delta[R]
  ) extends Operation {
    val operationType: OperationType.Edit.type = OperationType.Edit
    override val resourceIdOpt: Some[IdNumber] = Some(resourceId)
  }

  /** Describes an operation that deletes a resource.
    * @param performed See [[Operation.performed]].
    * @param resourceType See [[Operation.resourceType]].
    * @param resourceId See [[Operation.resourceIdOpt]].
    */
  case class Delete(
    performed: ZonedDateTime,
    resourceType: ResourceType,
    resourceId: IdNumber
  ) extends Operation {
    val operationType: OperationType.Delete.type = OperationType.Delete
    override val resourceIdOpt: Some[IdNumber] = Some(resourceId)
  }
}
