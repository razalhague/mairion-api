package org.penny_craal.mairion

import cats.data.EitherT
import cats.effect.IO
import cats.free.Free
import org.penny_craal.mairion.representations.IdNumber

import scala.language.higherKinds

/** This package defines operations available on a resource repository. */
package object resourcerepository {
  /** A free monad for operations on a resource repository.
    * @tparam A  type of the monadic value
    */
  type RRIO[A] = Free[ResourceRepository, A]

  /** The type returned by any of the BREAD requests on a resource repository.
    * @tparam R  the type of resource returned by the repository
    */
  type RRResult[R] = Map[IdNumber, R]

  /** Represents values that result from operations that may fail.
    * @tparam A  the type of value.
    */
  type Fallible[+A] = Either[MairionError, A]

  /** A monad transformer for monadic values that may fail.
    * @tparam M the type of monad
    * @tparam A the type of value
    */
  type FallibleT[M[_], A] = EitherT[M, MairionError, A]

  /** An IO value from an operation that may fail.
    * @tparam A the type of value
    */
  type FallibleIO[A] = FallibleT[IO, A]
}
