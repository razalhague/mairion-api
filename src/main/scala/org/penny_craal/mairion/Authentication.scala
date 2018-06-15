package org.penny_craal.mairion

import cats.data.{EitherT, Kleisli}
import cats.effect.IO
import org.http4s.{AuthedService, Request => HttpRequest}
import org.http4s.headers.Authorization
import org.http4s.server.AuthMiddleware
import org.penny_craal.mairion.resourcerepository.Credentials.BasicAuth
import org.penny_craal.mairion.resourcerepository._
import org.penny_craal.mairion.representations.IdNumber

/** Provides tools for handling authentication. */
object Authentication {
  /** The password hashing function.
    * @param password A plaintext string.
    * @return A string containing the password's hash.
    */
  def hashPassword(password: String): String = password.hashCode.toString // insecure, just for testing

  /** Returns the user's ID number if the request's authentication information is valid, None if no authentication
    * information, and error if something went wrong.
    */
  private val authUser: Kleisli[IO, HttpRequest[IO], Fallible[Option[IdNumber]]] = Kleisli { request =>
    val userId: FallibleIO[Option[IdNumber]] = request.headers get Authorization match {
      case None => EitherT.rightT(None)
      case Some(header) => for {
        credentials <- getCredentialsFromHeader(header)
        idFrrio = ResourceRepositoryAuth.getUserIdIfCredentialsAuthenticate(credentials)
        idOpt <- RrioSharedStateInterpreter.compileFallibleT(idFrrio)
      } yield idOpt
    }
    userId.value
  }

  /** Extracts credentials from the given header. */
  private def getCredentialsFromHeader(authorization: Authorization): FallibleIO[Credentials] = authorization match {
    case BasicAuth(user, pass) => EitherT.rightT(BasicAuth(user, pass))
    case _ => EitherT.leftT(MairionError.unsupportedAuthScheme(authorization.credentials.authScheme.value))
  }

  /** Handles situations where something went wrong with the authentication. */
  private val authenticationFailureService: AuthedService[MairionError, IO] = AuthedService[MairionError, IO] {
    case request => services.defaultErrorResponder(request.authInfo)
  }

  /** Turns an [[https://http4s.org/v0.18/api/org/http4s/authedservice$ AuthedService]] into an
    * [[https://http4s.org/v0.18/api/org/http4s/httpservice$ HttpService]].
    */
  val middleware: AuthMiddleware[IO, Option[IdNumber]] = AuthMiddleware(authUser, authenticationFailureService)
}
