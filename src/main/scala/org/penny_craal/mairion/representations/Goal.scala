package org.penny_craal.mairion.representations

/** Represents a goal set for some time period.
  * @param revision See [[Resource.Transactable.revision]]
  * @param taskId ID of the task that is associated with this goal.
  * @param periodId ID of the period that
  * @param condition The condition that determines if this goal is achieved.
  */
case class Goal(
  revision: IdNumber,
  taskId: IdNumber,
  periodId: IdNumber,
  condition: SuccessCondition
) extends Resource.Transactable
