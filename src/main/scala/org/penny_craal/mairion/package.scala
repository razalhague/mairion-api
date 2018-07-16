package org.penny_craal

import cats.data.Validated.{Invalid, Valid}
import cats.data.{NonEmptyList, ValidatedNel}
import cats.~>
import org.http4s.{ParseFailure, QueryParamDecoder, QueryParameterValue}
import org.http4s.dsl.impl.{LongVar, QueryParamDecoderMatcher}
import org.penny_craal.mairion.representations.IdNumber
import org.penny_craal.mairion.resourcerepository.{FallibleIO, FallibleT, RRIO, ResourceRepository}

/** Contains the Mairion REST API application and classes related to it. */
package object mairion {
  /** Determines if debug information communicated to the consumer of the API. */
  val debug = true

  /** Compiles a [[resourcerepository.RRIO]] value into a [[resourcerepository.FallibleIO]] value.
    * @param a The monadic value.
    * @tparam A The type of value contained by the monad.
    * @return The value wrapped in [[resourcerepository.FallibleIO]].
    */
  def compile[A](compiler: ResourceRepository ~> FallibleIO)(a: RRIO[A]): FallibleIO[A] =
    a foldMap compiler

  /** Compiles a [[resourcerepository.RRIO]] value that uses [[resourcerepository.FallibleT]] error handling into a
    * [[resourcerepository.FallibleIO]] value.
    * @param a The monadic value.
    * @tparam A The type of value contained by the monad.
    * @return The value wrapped in [[resourcerepository.FallibleIO]], with the error handling merged.
    */
  def compileFallibleT[A](compiler: ResourceRepository ~> FallibleIO)(a: FallibleT[RRIO, A]): FallibleIO[A] =
    compile(compiler)(a.value) subflatMap (x => x)

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
