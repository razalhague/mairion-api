package org.penny_craal.mairion

import java.time.ZonedDateTime

import cats.data.EitherT
import cats.effect.IO
import cats.~>
import org.penny_craal.mairion.resourcerepository._

/** An interpreter for turning [[resourcerepository.RRIO]] values into [[resourcerepository.FallibleIO]] values. Uses
  * synchronized shared state.
  */
object RrioSharedStateInterpreter extends (ResourceRepository ~> FallibleIO) {
  /** Variable for the shared state. */
  private var db = RrioStateInterpreter.initialState

  override def apply[A](valueRR: ResourceRepository[A]): FallibleIO[A] = EitherT(IO {
    val now = ZonedDateTime.now()
    db.synchronized {
      val (newDb, valueF) = RrioStateInterpreter.at(now)(valueRR).value.run(db).value
      db = newDb
      valueF
    }
  })
}
