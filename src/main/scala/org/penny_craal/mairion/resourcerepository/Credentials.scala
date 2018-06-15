package org.penny_craal.mairion.resourcerepository

import java.util.Base64

import org.http4s.AuthScheme
import org.http4s.headers.Authorization

/** Describes credentials passed along with the HTTP request. */
sealed trait Credentials

object Credentials {
  /** Credentials of type Basic.
    * @param userName The user's name.
    * @param password The user's password.
    */
  case class BasicAuth(
    userName: String,
    password: String
  ) extends Credentials

  object BasicAuth {
    def unapply(credentials: Credentials): Option[(String, String)] = credentials match {
      case ba: BasicAuth => Some((ba.userName, ba.password))
      case _ => None
    }

    /** Extracts the username and password from an Authorization header.
      * @param authorization The header.
      * @return A pair formed from the username and password.
      */
    def unapply(authorization: Authorization): Option[(String, String)] = {
      authorization.credentials.authScheme match {
        case AuthScheme.Basic =>
          val encodedAuth = authorization.value.replace("Basic ", "")
          val decodedAuth = new String(Base64.getDecoder.decode(encodedAuth), "UTF-8")
          val splitAuth = decodedAuth.split(':')
          if (splitAuth.length == 2) Some(splitAuth(0) -> splitAuth(1))
          else None
        case _ => None
      }
    }
  }
}

