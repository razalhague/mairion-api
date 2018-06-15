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

  /** Compiles a [[resourcerepository.RRIO]] value into a [[resourcerepository.FallibleIO]] value.
    * @param a The monadic value.
    * @tparam A The type of value contained by the monad.
    * @return The value wrapped in [[resourcerepository.FallibleIO]].
    */
  def compile[A](a: RRIO[A]): FallibleIO[A] =
    a foldMap this

  /** Compiles a [[resourcerepository.RRIO]] value that uses [[resourcerepository.FallibleT]] error handling into a
    * [[resourcerepository.FallibleIO]] value.
    * @param a The monadic value.
    * @tparam A The type of value contained by the monad.
    * @return The value wrapped in [[resourcerepository.FallibleIO]], with the error handling merged.
    */
  def compileFallibleT[A](a: FallibleT[RRIO, A]): FallibleIO[A] =
    compile(a.value) subflatMap (x => x)
}
