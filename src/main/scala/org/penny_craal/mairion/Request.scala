package org.penny_craal.mairion

import org.penny_craal.mairion.resourcerepository.Filter
import org.penny_craal.mairion.representations.{Delta, IdNumber, OperationType, Resource, ResourceType}

/** Describes a request made to the server.
  * @tparam R The type of result that is expected for this request.
  */
sealed trait Request[R <: Resource] {
  /** The type of operation this request would perform. */
  val operationType: OperationType

  /** The type of resource this request targets. */
  val resourceType: ResourceType
}

object Request {
  /** Describes a request that targets a resource collection. */
  sealed trait OnCollectionResource[R <: Resource] extends Request[R] {
    override val operationType: OperationType.CollectionResource
  }

  /** Describes a request that targets an individual resource. */
  sealed trait OnIndividualResource[R <: Resource] extends Request[R] {
    override val operationType: OperationType.IndividualResource
    val resourceId: IdNumber
  }

  /** See [[resourcerepository.ResourceRepository.browse]]. */
  case class Browse[R <: Resource](
    resourceType: ResourceType,
    filters: Seq[Filter],
  ) extends OnCollectionResource[R] {
    override val operationType: OperationType.Browse.type = OperationType.Browse
  }

  /** See [[resourcerepository.ResourceRepository.read]]. */
  case class Read[R <: Resource](
    resourceType: ResourceType,
    resourceId: IdNumber,
  ) extends OnIndividualResource[R] {
    override val operationType: OperationType.Read.type = OperationType.Read
  }

  /** See [[resourcerepository.ResourceRepository.edit]]. */
  case class Edit[R <: Resource](
    resourceType: ResourceType,
    resourceId: IdNumber,
    delta: Delta[R]
  ) extends OnIndividualResource[R] {
    override val operationType: OperationType.Edit.type = OperationType.Edit
  }

  /** See [[resourcerepository.ResourceRepository.add]]. */
  case class Add[R <: Resource](
    resourceType: ResourceType,
    resource: Any,
  ) extends OnCollectionResource[R] {
    override val operationType: OperationType.Add.type = OperationType.Add
  }

  /** See [[resourcerepository.ResourceRepository.delete]]. */
  case class Delete[R <: Resource](
    resourceType: ResourceType,
    resourceId: IdNumber,
  ) extends OnIndividualResource[R] {
    override val operationType: OperationType.Delete.type = OperationType.Delete
  }

  /** A request with identification information attached.
    * @param userId The identification information.
    * @param request The request.
    * @tparam R The type of request.
    */
  case class Identified[+R <: Request[_]](userId: Option[IdNumber], request: R)
}
