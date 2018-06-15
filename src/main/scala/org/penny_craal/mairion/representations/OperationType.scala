package org.penny_craal.mairion.representations

/** Describes a type of operation. */
sealed trait OperationType

object OperationType {
  /** Describes an operation that can be performed in a [[Transaction]]. */
  sealed trait Transactable extends OperationType

  /** Describes an operation that can be performed on a resource collection. */
  sealed trait CollectionResource extends OperationType

  /** Describes an operation that can be performed on an individual resource. */
  sealed trait IndividualResource extends OperationType

  /** Browse resources. */
  case object Browse extends CollectionResource

  /** Read a resource. */
  case object Read extends IndividualResource

  /** Edit an existing resource. */
  case object Edit extends Transactable with IndividualResource

  /** Add a new resource. */
  case object Add extends Transactable with CollectionResource

  /** Delete a resource. */
  case object Delete extends Transactable with IndividualResource
}
