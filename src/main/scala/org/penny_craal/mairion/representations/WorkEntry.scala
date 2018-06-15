package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Describes a period of time spent continuously working on a [[Task]].
  * @param revision See [[Resource.Transactable.revision]].
  * @param performerId The user who did the work.
  * @param begun The time when work began.
  * @param ended The time when work ended, None if work still ongoing.
  * @param description A description of the work.
  */
case class WorkEntry(
  revision: IdNumber,
  performerId: IdNumber,
  begun: ZonedDateTime,
  ended: Option[ZonedDateTime],
  description: Option[String],
) extends Resource.Transactable
