package org.penny_craal

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}
import org.http4s.dsl.impl.{LongVar, QueryParamDecoderMatcher}
import org.penny_craal.mairion.representations.IdNumber

/** Contains the Mairion REST API application and classes related to it. */
package object mairion {
  /** Determines if debug information communicated to the consumer of the API. */
  val debug = true

  /** Provides extensions to [[https://http4s.org/v0.18/api/org/http4s/dsl/http4sdsl Http4sDsl]]. */
  object dslextensions {
    implicit object IdNumberQueryParamDecoder extends QueryParamDecoder[IdNumber] {
      override def decode(param: QueryParameterValue): ValidatedNel[ParseFailure, IdNumber] = {
        try {
          Valid(IdNumber.unapply(param.value.toLong).get)
        } catch {
          case _: NumberFormatException => Invalid(NonEmptyList.of(ParseFailure("Not a valid number", param.value)))
          case _: NoSuchElementException => Invalid(NonEmptyList.of(ParseFailure("Not a valid ID number", param.value)))
        }
      }
    }

    /** Extractor of workspace query parameters. */
    object WorkspaceQueryParam extends QueryParamDecoderMatcher[IdNumber]("workspace")

    /** Extractor of group query parameters. */
    object GroupQueryParam extends QueryParamDecoderMatcher[IdNumber]("group")

    /** IdNumber Extractor from String. */
    object ResourceIdVar {
      /** Extracts an IdNumber from a String, if valid.
        * @param string The candidate IdNumber in a String.
        * @return Some(idNumber) if valid, None if invalid.
        */
      def unapply(string: String): Option[IdNumber] = string match {
        case LongVar(IdNumber(idNumber)) => Some(idNumber)
        case _ => None
      }
    }
  }
}
