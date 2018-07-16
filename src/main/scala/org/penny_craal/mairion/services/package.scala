package org.penny_craal.mairion

import cats.data.{EitherT, NonEmptyList}
import cats.effect.IO
import cats.~>
import io.circe.{Encoder, Json}
import io.circe.syntax._
import io.circe.generic.auto._
import org.http4s.circe._
import org.http4s.headers.`WWW-Authenticate`
import org.http4s.{AuthedRequest, AuthedService, Challenge, Headers, Response, Status}
import org.penny_craal.mairion.Request.Identified
import org.penny_craal.mairion.resourcerepository.MairionError.id
import org.penny_craal.mairion.resourcerepository._
import org.penny_craal.mairion.representations.{IdNumber, Resource, OperationType => OT}

/** Provides Blaze services for various resources, a function for creating them, and various default functions for
  * processing the request.
  */
package object services {
  /** A partial function that parses a set of [[https://http4s.org/v0.18/api/org/http4s/authedrequest AuthedRequest]]s
    * into [[Request]]s. Cases not handled by the function may be parsed by other services.
    * @tparam R The type parameter for [[Request]] objects this function produces.
    */
  type HttpRequestParser[R <: Resource.Plain] =
    PartialFunction[AuthedRequest[IO, Option[IdNumber]], FallibleIO[Request[R]]]

  /** A partial function that processes an [[Request.Identified]] into a [[resourcerepository.RRResult]] contained by
    * [[resourcerepository.RRIO]].
    * @tparam R The type parameter for [[RRResult]] objects this function produces.
    */
  type ApiRequestProcessor[R <: Resource.Plain] = PartialFunction[Identified[Request[R]], RRIO[RRResult[R]]]

  /** A function that presents a [[resourcerepository.RRResult]] as a
    * [[https://http4s.org/v0.18/api/org/http4s/response Response]] contained by [[resourcerepository.FallibleIO]],
    * with the help of a [[Request]] and an [[https://circe.github.io/circe/api/io/circe/Encoder.html Encoder]].
    * @tparam R The type parameter for the inputs to the function.
    */
  type SuccessResponder[R <: Resource.Plain] = (RRResult[R], Request[R], Encoder[R]) => FallibleIO[Response[IO]]

  /** A function that presents a [[resourcerepository.MairionError]] as a
    * [[https://http4s.org/v0.18/api/org/http4s/response Response]] in
    * [[https://typelevel.org/cats-effect/api/cats/effect/IO.html IO]].
    */
  type ErrorResponder = MairionError => IO[Response[IO]]

  /** Returns an [[https://http4s.org/v0.18/api/org/http4s/authedservice$ AuthedService]] constructed from the
    * parameters.
    * @param httpRequestParser   See [[HttpRequestParser]].
    * @param apiRequestProcessor See [[ApiRequestProcessor]]. Any cases not handled by this processor will be processed
    *                            with [[defaultProcessor]].
    * @param successResponder    See [[SuccessResponder]].
    * @param errorResponder      See [[ErrorResponder]].
    * @param resourceEncoder     An encoder for the resulting [[representations.Resource.Plain]]s.
    * @tparam R The type of [[representations.Resource.Plain]] produced by the returned service.
    * @return An AuthedService.
    */
  def makeResourceService[R <: Resource.Plain](
    httpRequestParser: HttpRequestParser[R],
    compiler: ResourceRepository ~> FallibleIO,
    apiRequestProcessor: ApiRequestProcessor[R] = PartialFunction.empty,
    successResponder: SuccessResponder[R] = defaultSuccessResponder[R],
    errorResponder: ErrorResponder = defaultErrorResponder
  )(implicit resourceEncoder: Encoder[R]): AuthedService[Option[IdNumber], IO] = AuthedService[Option[IdNumber], IO] {
    case httpRequest if httpRequestParser.isDefinedAt(httpRequest) =>
      val responseOrError: FallibleIO[Response[IO]] = for {
        request <- httpRequestParser(httpRequest)
        idRequest = Request.Identified(httpRequest.authInfo, request)
        _ <- verifyRequestIsAuthorized(idRequest, compiler)
        results <- processRequest(apiRequestProcessor, idRequest, compiler)
        response <- successResponder(results, request, resourceEncoder)
      } yield response
      responseOrError valueOrF errorResponder
  }

  /** Checks that the request is authorized.
    * @param idRequest The request to be checked for authotization.
    * @tparam R The type of result the request would get.
    * @return A successful FallibleIO of Unit if the request passed authorization, a failed one if not.
    */
  private def verifyRequestIsAuthorized[R <: Resource.Plain](
    idRequest: Identified[Request[R]],
    compiler: ResourceRepository ~> FallibleIO
  ): FallibleIO[Unit] = {
    val isAuthorizedFtRrio = ResourceRepositoryAuth.isRequestAuthorized(idRequest)
    val isAuthorizedFio = compileFallibleT(compiler)(isAuthorizedFtRrio)
    isAuthorizedFio subflatMap {
      case true => Right(())
      case false => Left(MairionError.unauthorized)
    }
  }

  /** Returns the result of processing the given request.
    * @param processor Processor for handling special cases.
    * @param idRequest The request to be processed.
    * @tparam R The type of result.
    * @return A [[RRResult]] of R in [[FallibleIO]]
    */
  private def processRequest[R <: Resource.Plain](
    processor: ApiRequestProcessor[R],
    idRequest: Identified[Request[R]],
    compiler: ResourceRepository ~> FallibleIO
  ): FallibleIO[RRResult[R]] = {
    val totalProcessor = processor orElse defaultProcessor
    val resultRrio = totalProcessor(idRequest)
    compile(compiler)(resultRrio)
  }

  /** Returns a [[SuccessResponder]] for the given type, which wraps up the result in a HTTP response with the
    * appropriate status code.
    * @tparam R The type of result the returned responder handles.
    * @return A [[SuccessResponder]]. Status: Created for Adds, Ok for all other requests.
    */
  def defaultSuccessResponder[R <: Resource.Plain]: SuccessResponder[R] = { (result, request, resourceEncoder) =>
    val encodeMapping: (IdNumber, R) => (Long, Json) = _.value -> resourceEncoder(_)
    implicit val resultEncoder: Encoder[RRResult[R]] = Encoder[Map[Long, Json]] contramap (_ map encodeMapping.tupled)

    request.operationType match {
      case OT.Browse
         | OT.Read
         | OT.Edit
         | OT.Delete
        => EitherT.right(Response(Status.Ok).withBody(result.asJson))
      case OT.Add => EitherT.right(Response(Status.Created).withBody(result.asJson))
    }
  }

  /** Sends the error to the API's consumer if the server is in debug mode.
    * @param response The response to inject the error into.
    * @param error The error.
    * @return A response.
    */
  private def insertBodyIfDebug(response: Response[IO], error: MairionError): IO[Response[IO]] =
    if (debug) response.withBody(error.asJson) else IO(response)

  /** The default [[ErrorResponder]], wraps up errors in a HTTP response, with an appropriate status. */
  val defaultErrorResponder: ErrorResponder = (error) => error.errorId match {
    case id.notAuthenticated
       | id.unsupportedAuthScheme
      => val authenticateHeaders = Headers(`WWW-Authenticate`(NonEmptyList.of(Challenge("Basic", ""))))
         IO(Response[IO](Status.Unauthorized).copy(headers = authenticateHeaders))
    case id.unauthorized => IO(Response(Status.Forbidden))
    case id.missingParam => Response(Status.BadRequest).withBody(error.asJson)
    case id.unexpectedJsonMember
       | id.wrongType
       | id.emptyDelta
       | id.notAnObject
       | id.duplicateEmail
      => Response(Status.UnprocessableEntity).withBody(error.asJson)
    case id.notImplemented => insertBodyIfDebug(Response(Status.NotImplemented), error)
    case _ => insertBodyIfDebug(Response(Status.InternalServerError), error)
  }

  /** Returns an [[ApiRequestProcessor]] for R, which creates a corresponding RRIO operation from the provided Request.
    * @tparam R The type of result.
    * @return A simple [[ApiRequestProcessor]].
    */
  def defaultProcessor[R <: Resource.Plain]: ApiRequestProcessor[R] = {
    case Identified(_, request) => ResourceRepository.performRequest[R](request)
  }
}
