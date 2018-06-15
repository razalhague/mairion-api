package org.penny_craal.mairion

import cats.data.Kleisli
import org.penny_craal.mairion.resourcerepository.Fallible

/** Provides case classes and objects that represent the resources handled by the system, as subclasses of
  * [[representations.Resource]]. */
package object representations {
  /** A Kleisli-function that produces an updated version of an object, and may fail.
    * @tparam A The type of object it updates.
    */
  type Delta[A] = Kleisli[Fallible, A, A]
}
