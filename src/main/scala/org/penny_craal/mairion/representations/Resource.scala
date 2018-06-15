package org.penny_craal.mairion.representations

/** Parent trait for all Resources. */
trait Resource

object Resource {
  /** A [[Resource]] that can be received or transmitted directly through the API. */
  trait Plain extends Resource

  /** A [[Resource]] that can be accessed and modified through transactions. */
  trait Transactable extends Resource {
    /** The revision number of this resource. */
    val revision: IdNumber
  }
}
