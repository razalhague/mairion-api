package org.penny_craal.mairion.representations

/** A group of users in a [[Workspace]].
  * @param name Name of the group.
  * @param wsId ID of the workspace this group belongs to.
  * @param members A list of members in this group.
  * @param owners A list of users who are considered to own this group.
  */
case class Group(
  name: String,
  wsId: IdNumber,
  members: Seq[IdNumber],
  owners: Seq[IdNumber],
) extends Resource.Plain
