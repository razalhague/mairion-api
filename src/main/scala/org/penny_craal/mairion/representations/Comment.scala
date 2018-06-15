package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Represents a comment made on some resource.
  * @param revision
  * @param resourceType Which type of [[Resource]] this comment belongs to.
  * @param resourceId Identifies which resource of the specified resource type this comment was made on.
  * @param parentRevision Identifies which revision of the resource this comment was made on.
  * @param authorId The ID of this comment's author.
  * @param created The time at which this comment was made.
  * @param content The content of this comment.
  */
case class Comment(
  revision: IdNumber,
  resourceType: ResourceType.Transactable,
  resourceId: IdNumber,
  parentRevision: IdNumber,
  authorId: IdNumber,
  created: ZonedDateTime,
  content: String
) extends Resource.Transactable
