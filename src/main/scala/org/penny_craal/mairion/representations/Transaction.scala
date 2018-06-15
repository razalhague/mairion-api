package org.penny_craal.mairion.representations

import java.time.ZonedDateTime

import cats.data.NonEmptyList

/** Describes a transaction.
  * @param wsId The [[Workspace]] this transaction is a part of.
  * @param authorId The ID of the user who made this transaction.
  * @param commit The time this transaction was committed.
  * @param ops A list of operations that form this transaction.
  */
case class Transaction(
  wsId: IdNumber,
  authorId: IdNumber,
  commit: ZonedDateTime,
  ops: NonEmptyList[Operation],
) extends Resource.Plain
