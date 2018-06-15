package org.penny_craal.mairion.resourcerepository

import org.penny_craal.mairion.representations.{IdNumber, ResourceType}

/** Represents errors encountered during execution of a request.
  * @param errorId A unique identifier string for the type of error, meant for looking up localized error messages.
  * @param message A default error message in English with references to supplementary data encoded in the form
  *            $[NAME], where NAME is a key in the supplementary parameter.
  * @param supplementary Data that varies between instances of errors with the same ID. Empty by default. Values must
  *                      not contains character sequences that match
  */
case class MairionError(
  errorId: String,
  message: String,
  supplementary: Map[String, String] = Map.empty
) {
  /** Returns an error string with the supplementary data filled in, ready to be displayed to the user. */
  def errorString: String = MairionError.render(message, supplementary)
}

object MairionError {
  /** Renders the provided supplementary data into the message template.
    * @param message A template message with references to supplementary data.
    * @param supplementary Key-value pairs of supplementary data, to be inserted into the message template.
    * @return An error string, ready to be displayed.
    */
  def render(message: String, supplementary: Map[String, String]): String =
    supplementary
      .map { case(k, v) => "$[" + k + "]" -> v }
      .foldLeft(message) { case (accumulator, (reference, value)) => accumulator.replace(reference, value) }

  /** All the identifier strings used in the API. */
  object id {
    val unsupportedAuthScheme = "unsupportedAuthScheme"
    val notAnObject = "notAnObject"
    val unexpectedJsonMember = "unexpectedJsonMember"
    val wrongType = "element"
    val notAuthenticated = "notAuthenticated"
    val authenticated = "authenticated"
    val unrecognizedExceptionThrown = "unrecognizedExceptionThrown"
    val unsupportedEdit = "unsupportedEdit"
    val pathObjectMismatch = "pathObjectMismatch"
    val internal = "internal"
    val missingParam = "missingParam"
    val resourceNotFound = "resourceNotFound"
    val duplicateEmail = "duplicateEmail"
    val unauthorized = "unauthorized"
    val emptyDelta = "emptyDelta"
    val notImplemented = "notImplemented"
    val noJurisdiction = "noJurisdiction"
  }

  def noJurisdiction(request: String) =
    MairionError(id.noJurisdiction, "no workspace has jurisdiction over $[request]", Map("request" -> request))

  def notImplemented(feature: String) = MairionError(
    id.notImplemented,
    "$[feature] has not been implemented",
    Map("feature" -> feature)
  )

  def emptyDelta = MairionError(id.emptyDelta, "resource delta cannot be empty")

  def unauthorized = MairionError(id.unauthorized, "This user is not authorized to make this request")

  def duplicateEmail(email: String) = MairionError(id.duplicateEmail, "email $[email] is taken", Map("email" -> email))

  def resourceNotFound(resourceType: ResourceType, resourceId: IdNumber) = MairionError(
    id.resourceNotFound,
    "Could not find resource of type $[resourceType] with id $[resId]",
    Map("resourceType" -> resourceType.toString, "resourceId" -> resourceId.toString)
  )

  def unsupportedAuthScheme(header: String) =
    MairionError(id.unsupportedAuthScheme, "Unsupported auth scheme in header $[header]", Map("header" -> header))

  def notAnObject(element: String) =
    MairionError(id.notAnObject, "$[element] is not a JSON object", Map("element" -> element))

  def unexpectedJsonMember(elementName: String, memberName: String) = MairionError(
    id.unexpectedJsonMember,
    "$[objectName] should not contain member $[memberName]",
    Map("memberName" -> memberName, "objectName" -> elementName)
  )

  def wrongType(elementName: String, expectedType: String, actualType: String) = MairionError(
    id.wrongType,
    "$[elementName] should be of type $[elementType], but was $[actualType]",
    Map("elementName" -> elementName, "expectedType" -> expectedType, "actualType" -> actualType)
  )

  def notAuthenticated = MairionError(id.notAuthenticated, "This endpoint requires authentication")

  def authenticated = MairionError(id.authenticated, "This endpoint mandates no authentication")

  def unrecognizedExceptionThrown(throwable: Throwable) = MairionError(
    id.unrecognizedExceptionThrown,
    "An unrecognized exception was thrown during the execution of this request",
    Map("exception" -> (throwable.toString + "\n" + throwable.getStackTrace.map(_.toString).mkString("\n")))
  )

  def unsupportedEdit(member: String) =
    MairionError(id.unsupportedEdit, "Changing $[member] is not supported", Map("member" -> member))

  def pathObjectMismatch(member: String) = MairionError(
    id.pathObjectMismatch,
    "the value of member $[member] does not match the corresponding value in path",
    Map("member" -> member)
  )

  def internal(description: String) = MairionError(id.internal, description)

  def missingParam(paramName: String) =
    MairionError(id.missingParam, "The request is missing the parameter '$[paramName]'", Map("paramName" -> paramName))
}
