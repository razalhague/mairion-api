package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

/** Describes the termination of a [[WorkspaceMembership]]. */
sealed trait Termination extends Resource.Plain {
  /** The time when the membership was terminated. */
  val time: Option[ZonedDateTime]

  /** The reason given for this termination. */
  val reason: Option[String]

  /** Returns a copy of this termination with the given time attached to it. */
  def withTime(time: Option[ZonedDateTime]): Termination
}

object Termination {
  /** The membership was terminated with a resignation. */
  case class Resignation(
    time: Option[ZonedDateTime],
    reason: Option[String]
  ) extends Termination {
    override def withTime(newTime: Option[ZonedDateTime]): Termination = copy(time = newTime)
  }

  /** The membership was terminated with a dismissal.
    * @param dismissor ID of the user who dismissed this user from the workspace.
    */
  case class Dismissal(
    time: Option[ZonedDateTime],
    reason: Option[String],
    dismissor: IdNumber
  ) extends Termination {
    override def withTime(newTime: Option[ZonedDateTime]): Termination = copy(time = newTime)
  }

  /** A version of a Termination object that can be passed in through the API. */
  sealed trait In extends Resource.Plain {
    /** Returns a proper [[Termination]] object with the appropriate details attached.
      * @param requesterId Identifies who requested this termination.
      * @return A full Termination object.
      */
    def full(requesterId: IdNumber): Termination
  }

  object In {
    /** An In implementation for [[Termination.Resignation]]. */
    case class Resignation(
      note: Option[String]
    ) extends In {
      override def full(requesterId: IdNumber): Termination.Resignation = Termination.Resignation(None, note)
    }

    /** An In implementation for [[Termination.Dismissal]]. */
    case class Dismissal(
      note: Option[String],
      dismissee: IdNumber,
    ) extends In {
      override def full(requesterId: IdNumber): Termination.Dismissal = Termination.Dismissal(None, note, requesterId)
    }
  }
}
