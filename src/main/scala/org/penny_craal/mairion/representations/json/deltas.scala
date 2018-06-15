package org.penny_craal.mairion.representations
package json

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

import cats.data.Kleisli
import cats.implicits._
import io.circe.{Decoder, Json, JsonNumber, JsonObject}
import org.penny_craal.mairion.Authentication
import org.penny_craal.mairion.resourcerepository.{Fallible, MairionError}

import scala.util.Try

object deltas {
  /** A function that produces an updated version of an object, and may fail. Meant for updating individual members of
    * an object, and meant to be composed into a larger [[Delta]].
    * @tparam A The type of object it updates.
    */
  private type MemberDelta[A] = A => Fallible[A]

  /** A function that returns a [[MemberDelta]] created from the given Json value.
    * @tparam A The type of objects that the produced member deltas update.
    */
  private type MemberDeltaFactory[A] = Json => MemberDelta[A]

  /**
    * A partial function that returns a [[MemberDeltaFactory]] for the given member.
    * @tparam A The type of objects that the returned [[MemberDeltaFactory]]'s [[MemberDelta]]s update.
    */
  private type MemberDeltaDecoder[A] = PartialFunction[String, MemberDeltaFactory[A]]

  /** Returns a Decoder for a [[Delta]] of A.
    * @param memberDeltaDecoder See [[MemberDeltaDecoder]]. Passing a JsonObject to the returned Parser that has members
    *                          for which this parameter does not define cases for will result in failing [[Delta]]s.
    * @tparam A The type of objects that the returned Decoder's [[Delta]]s update.
    * @return A decoder of deltas.
    */
  private def makeDeltaDecoder[A](memberDeltaDecoder: MemberDeltaDecoder[A], objectName: String): Decoder[Delta[A]] = {
    val parseMemberDelta: (String, Json) => Delta[A] = { case (memberName, newValue) =>
      val totalMemberDeltaDecoder = memberDeltaDecoder orElse memberDeltaParseErrorCase(objectName)
      val memberDelta = totalMemberDeltaDecoder(memberName)(newValue)
      Kleisli(memberDelta)
    }
    Decoder[JsonObject] map (_.toList) emap {
      case Nil => Left("empty delta")
      case jsonMemberList => Right(jsonMemberList map parseMemberDelta.tupled reduce (_ andThen _))
    }
  }

  /** Returns a [[MemberDeltaDecoder]] that only produces failing [[MemberDelta]]s, which are meant to be composed with
    * orElse to a partial function that parses the successful cases.
    * @param objectName The name of the object being parsed, to be used in the error.
    * @tparam R The type of MemberDeltaParser this function returns.
    * @return A [[MemberDeltaDecoder]] that only produces failing [[MemberDelta]]s.
    */
  private def memberDeltaParseErrorCase[R](objectName: String): MemberDeltaDecoder[R] = {
    case member => _ => _ => Left(MairionError.unexpectedJsonMember(objectName, member))
  }

  /** Folds a Json value into a String describing the value's type. */
  private object TypeFolder extends Json.Folder[String] {
    override def onNull: String = "Null"
    override def onBoolean(value: Boolean): String = "Boolean"
    override def onNumber(value: JsonNumber): String = "Number"
    override def onString(value: String): String = "String"
    override def onArray(value: Vector[Json]): String = "Array"
    override def onObject(value: JsonObject): String = "Object"
  }

  /** Helper methods for extracting typed values from a Json value, with error messages attached if unexpected type.
    * @param jsonVal The Json value to extract data from.
    */
  implicit class JsonTypeHelper(val jsonVal: Json) extends AnyVal {
    /** Returns the type of the Json value as a String. */
    def typeString: String = jsonVal foldWith TypeFolder

    /** Returns a String if the Json value contains one, or an error.
      * @param elementName The name of the Json element, to be used in the error.
      * @return String or error.
      */
    def asStringF(elementName: String): Fallible[String] =
      jsonVal.asString toRight MairionError.wrongType(elementName, "String", typeString)

    /** Returns a Boolean if the Json value contains one, or an error.
      * @param elementName The name of the Json element, to be used in the error.
      * @return Boolean or error.
      */
    def asBooleanF(elementName: String): Fallible[Boolean] =
      jsonVal.asBoolean toRight MairionError.wrongType(elementName, "Boolean", typeString)

    /** Returns a ZonedDateTime if the Json value contains a String that can be parsed into one, or an error.
      * @param elementName The name of the Json element, to be used in the error.
      * @return ZonedDateTime or error.
      */
    def asDateF(elementName: String): Fallible[ZonedDateTime] = {
      val dateFormatString = "ISO 8601 combined date and time in UTC as a String"
      val dateOpt = for {
        dateString <- jsonVal.asString
        date <- Try(ZonedDateTime.parse(dateString, DateTimeFormatter.ISO_INSTANT)).toOption
      } yield date
      dateOpt toRight MairionError.wrongType(elementName, dateFormatString, typeString)
    }
  }

  private val userMemberDeltaDecoder: MemberDeltaDecoder[User] = {
    case "name"     => v => user => v.asStringF("name") map (name => user.copy(name = name))
    case "password" => v => user => v.asStringF("password") map (pw =>
      user.copy(hashedPassword = Authentication.hashPassword(pw))
    )
    case "email"    => v => user => v.asStringF("description") map (email => user.copy(email = email))
  }

  private val workspaceMemberDeltaParser: PartialFunction[String, Json => Workspace => Fallible[Workspace]] = {
    case "name"         => v => ws => v.asStringF("name") map (name => ws.copy(name = name))
    case "description"  => v => ws => v.asStringF("description") map (desc => ws.copy(description = desc))
  }

  implicit val userDeltaDecoder: Decoder[Delta[User]] = makeDeltaDecoder(userMemberDeltaDecoder, "request body")
  implicit val workspaceDeltaDecoder: Decoder[Delta[Workspace]] =
    makeDeltaDecoder(workspaceMemberDeltaParser, "request body")
}
