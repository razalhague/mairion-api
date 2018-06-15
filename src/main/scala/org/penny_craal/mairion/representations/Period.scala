package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Describes a period of time.
  * @param revision Revision number.
  * @param name Name of this period.
  * @param start Start point.
  * @param end End point.
  * @param description A description of this period.
  */
case class Period(
  revision: IdNumber,
  name: String,
  start: ZonedDateTime,
  end: ZonedDateTime,
  description: Option[String]
) extends Resource.Transactable
