package org.penny_craal.mairion.representations

/** An ID number.
  * @param value The ID as a Long.
  */
case class IdNumber private (value: Long) extends AnyVal

object IdNumber {
  /** Smallest possible ID number. */
  val minimum: IdNumber = unapply(1).get

  /** Extracts an IdNumber from a Long, if it is a valid ID number. */
  def unapply(long: Long): Option[IdNumber] =
    if (long > 0) Some(IdNumber(long))
    else None

  /** Extracts the Long value. */
  def unapply(id: IdNumber): Some[Long] = Some(id.value)
}
