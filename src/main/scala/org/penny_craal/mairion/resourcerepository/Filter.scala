package org.penny_craal.mairion.resourcerepository

import org.penny_craal.mairion.representations.{IdNumber, ResourceType}

/** Describes how the results of a Browse requests should be filtered. */
sealed trait Filter

object Filter {
  /** Only show results that match the provided e-mail.
    * @param email the email
    */
  case class ByEmail(
    email: String
  ) extends Filter

  /** Only show results that contain a reference to the specified resource.
    * @param resourceType the type of resource to filter by
    * @param resourceId the ID of the resource to filter by
    */
  case class ById(
    resourceType: ResourceType,
    resourceId: IdNumber
  ) extends Filter
}
