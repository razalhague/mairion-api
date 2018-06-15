package org.penny_craal.mairion.representations

import java.time.Duration

/** Describes  */
sealed trait SuccessCondition

object SuccessCondition {
  /** Successful if more work has been performed on the relevant task than specified here.
    * @param duration Minimum amount of work for success.
    */
  case class MoreThan(duration: Duration) extends SuccessCondition

  /** Successful if the relevant task has been completed. */
  case object ToCompletion extends SuccessCondition

  /** Successful if any of the conditions are successful.
    * @param conditions A list of conditions.
    */
  case class Or(conditions: SuccessCondition*) extends SuccessCondition

  /** Successful if all of the conditions are successful
    * @param conditions A list of conditions.
    */
  case class And(conditions: SuccessCondition*) extends SuccessCondition
}
