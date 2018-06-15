package org.penny_craal.mairion.representations

/** Describes a user of the system.
  * @param name Name of the user.
  * @param hashedPassword The user's password, hashed.
  * @param email The user's email.
  */
case class User(
  name: String,
  hashedPassword: String,
  email: String,
) extends Resource.Plain

object User {
  /** A version of [[User]] that may be passed in through the API.
    * @param name See [[User]].
    * @param password The user's password in plain text.
    * @param email See [[User]].
    */
  case class In(
    name: String,
    password: String,
    email: String,
  ) extends Resource.Plain
}
