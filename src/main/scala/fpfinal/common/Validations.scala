package fpfinal.common

import cats.Order
import cats.data.{NonEmptyChain, NonEmptySet, Validated}
import fpfinal.app.Configuration.IsValid
import cats.implicits._

import scala.collection.immutable.SortedSet

/**
 * Set of simple validations that can be reused across the different models.
 */
object Validations {
  /**
    * TODO #1: Check that this String's length does not exceed the provided limit.
    */
  def maxLength(s: String, n: Int): IsValid[String] =
    Validated.condNec(s.size <= n, s, "String too long")

  /**
    * TODO #2: Turn this String into a validated double
    */
  def double(s: String): IsValid[Double] =
    s.toDoubleOption match {
      case Some(value) => Validated.validNec(value)
      case None => Validated.invalidNec("Cannot convert string to double")
    }

  /**
   * Validates that a Double is >= 0
   */
  def nonNegative(x: Double): IsValid[Double] =
    Validated.condNec(x >= 0, x, s"Double should be nonnegative")

  /**
   * Validates that a list is non-empty and converts it to a NonEmptySet.
   */
  def nonEmptySet[A: Order](list: List[A]): IsValid[NonEmptySet[A]] =
    Validated.fromOption(
      NonEmptySet.fromSet(SortedSet.from(list)(Order[A].toOrdering)),
      NonEmptyChain("List should be non-empty")
    )

  /**
   * Validates that a String is non-empty.
   */
  def nonEmptyString(s: String): IsValid[String] =
    Validated.condNec(s.nonEmpty, s, "String should be non-empty")

  /**
   * Validates that a String only contains letters.
   */
  def allLetters(s: String): IsValid[String] =
    Validated.condNec(s.forall(_.isLetter), s, "String should be all letters")

  def allNonEmptyStrings(strings: List[String]): IsValid[List[String]] =
    Validated.condNec(strings.forall(nonEmptyString(_).isValid), strings, "Not all strings are non empty")

}
