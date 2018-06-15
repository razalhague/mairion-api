package org.penny_craal.mairion.representations

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import cats.effect.IO
import io.circe.{Decoder, Encoder}
import io.circe.java8.time.encodeZonedDateTime
import io.circe.java8.time.decodeZonedDateTime
import io.circe.generic.auto._
import io.circe.generic.semiauto.deriveEncoder
import org.http4s.circe.jsonOf
import org.http4s.EntityDecoder
import org.penny_craal.mairion.representations.Termination.In

/** Provides tools for dealing with JSON data. */
package object json {
  private def decodeIdNumber(long: Long): Either[String, IdNumber] =
    IdNumber.unapply(long) toRight "Invalid IdNumber"

  /** Implicits for encoding, decoding and otherwise dealing with Json values. */
  object implicits {
    import deltas._
    // Circe decoders
    implicit val idNumberDecoder: Decoder[IdNumber] = Decoder[Long] emap decodeIdNumber
    implicit val zonedDateTimeDecoder: Decoder[ZonedDateTime] = decodeZonedDateTime(DateTimeFormatter.ISO_INSTANT)

    // http4s decoders
    implicit val userInDecoder: EntityDecoder[IO, User.In] = jsonOf[IO, User.In]
    implicit val dismissalInDecoder: EntityDecoder[IO, In.Dismissal] = jsonOf[IO, In.Dismissal]
    implicit val resignationInDecoder: EntityDecoder[IO, In.Resignation] = jsonOf[IO, In.Resignation]
    implicit val workspaceDecoder: EntityDecoder[IO, Workspace] = jsonOf[IO, Workspace]
    implicit val workspaceDeltaEntityDecoder: EntityDecoder[IO, Delta[Workspace]] = jsonOf[IO, Delta[Workspace]]
    implicit val userDeltaEntityDecoder: EntityDecoder[IO, Delta[User]] = jsonOf[IO, Delta[User]]

    // Circe encoders
    implicit val idNumberEncoder: Encoder[IdNumber] = Encoder[Long] contramap (_.value)
    implicit val zonedDateTimeEncoder: Encoder[ZonedDateTime] = encodeZonedDateTime(DateTimeFormatter.ISO_INSTANT)
    implicit val userEncoder: Encoder[User] = deriveEncoder[User] mapJsonObject (_.remove("hashedPassword"))
    implicit val workspaceEncoder: Encoder[Workspace] = deriveEncoder
    implicit val workspaceMembershipEncoder: Encoder[WorkspaceMembership] = deriveEncoder
  }
}
