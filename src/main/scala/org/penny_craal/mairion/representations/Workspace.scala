package org.penny_craal.mairion.representations

/** Describes a workspace.
  * @param name Name of the workspace.
  * @param description A description of the workspace.
  */
case class Workspace(
  name: String,
  description: String,
) extends Resource.Plain
