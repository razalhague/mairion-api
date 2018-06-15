package org.penny_craal.mairion.representations

/** Describes a task.
  * @param revision Revision number.
  * @param name Name of the task.
  * @param status The task's [[Task.Status]].
  * @param description A description of the task.
  * @param parentId The task's parent, if any.
  */
case class Task(
  revision: IdNumber,
  name: String,
  status: Task.Status,
  description: Option[String],
  parentId: Option[IdNumber],
) extends Resource.Transactable

object Task {
  /** Describes a task's status. */
  sealed trait Status

  /** The task has been created, and is ready for progress. */
  case object Open extends Status

  /** The task cannot be progressed for some reason. */
  case object Blocked extends Status

  /** The task has been completed. */
  case object Completed extends Status

  /** The task has been cancelled. */
  case object Cancelled extends Status
}
