/*
 * Copyright (c) Neo4j Sweden AB (http://neo4j.com)
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.opencypher.v9_0.ast.semantics

import org.opencypher.v9_0.ast.semantics.SemanticCheck.when
import org.opencypher.v9_0.expressions.Expression.SemanticContext
import org.opencypher.v9_0.util.ErrorMessageProvider
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.NotImplementedErrorMessageProvider

sealed trait SemanticCheck {

  /** Runs `this` check.
   *
   * If you need to call [[run]] from within another [[SemanticCheck]], use [[SemanticCheck.fromContext]] to retrieve the current context.
   *
   * @param state   Initial semantic state.
   * @param context Context containing dependencies.
   * @return Final semantic state and any errors produced while running `this` check.
   */
  def run(state: SemanticState, context: SemanticCheckContext): SemanticCheckResult = {
    SemanticCheckInterpreter.runCheck(this, state, context)
  }

  @deprecated(message = "Use `run` instead", since = "5.0")
  def apply(state: SemanticState): SemanticCheckResult = {
    run(state, SemanticCheckContext.default)
  }

  /** Creates a new combined check which runs `this` followed by `next`.
   * 
   * {{{
   * val check = first chain second chain third
   * }}}
   */
  def chain(next: SemanticCheck): SemanticCheck = {
    for {
      a <- this
      b <- next
    } yield SemanticCheckResult(b.state, a.errors ++ b.errors)
  }

  /** Creates a new combined check which runs `this` followed by `next`, but only if there were no errors.
   * 
   * If `this` produces any errors, `next` is skipped.
   */
  def ifOkChain(next: => SemanticCheck): SemanticCheck = {
    for {
      a <- this
      b <- when(a.errors.isEmpty)(next)
    } yield SemanticCheckResult(b.state, a.errors ++ b.errors)
  }

  /** Creates a new check which applies `f` to the result of `this` check.
   * 
   * Together with [[flatMap]] enables for-comprehension syntax.
   */
  def map(f: SemanticCheckResult => SemanticCheckResult): SemanticCheck = SemanticCheck.Map(this, f)

  /** Creates a new check which runs `this` followed by the check returned from applying `f` to the result of `this` check.
   * 
   * Together with [[map]] enables for-comprehension syntax:
   * {{{
   * for {
   *   res1 <- firstCheck
   *   res2 <- secondCheck
   * } yield SemanticCheckResult(res2.state, res1.errors ++ res2.errors)
   * }}}
   */
  def flatMap(f: SemanticCheckResult => SemanticCheck): SemanticCheck = SemanticCheck.FlatMap(this, f)

  /** Attaches a debug string to `this` check. */
  def annotate(annotation: => String): SemanticCheck = {
    if (SemanticCheck.DEBUG_ENABLED)
      SemanticCheck.Annotated(this, annotation)
    else
      this
  }

  /** Alias for [[annotate]]. */
  def :|(annotation: => String): SemanticCheck = annotate(annotation)

  /** Alias for [[annotate]]. */
  def |:(annotation: => String): SemanticCheck = annotate(annotation)
}

object SemanticCheck {

  /** Check which doesn't change state and does not produce any errors. */
  val success: SemanticCheck = fromFunction(SemanticCheckResult.success)

  /** Creates a check which doesn't change state but produces an error `error`. */
  def error(error: SemanticErrorDef): SemanticCheck = fromFunction(SemanticCheckResult.error(_, error))

  /** Creates a check from function. */
  def fromFunction(f: SemanticState => SemanticCheckResult): SemanticCheck = Leaf(f)

  /** Alias for [[success]]. */
  def getState: SemanticCheck = success

  /** Creates a check which changes the current state to `s`. Does not produce any errors. */
  def setState(s: SemanticState): SemanticCheck = fromFunction(_ => SemanticCheckResult.success(s))

  /** Creates the next check from the current state. */
  def fromState(f: SemanticState => SemanticCheck): SemanticCheck = success.flatMap(res => f(res.state))

  /** Creates the next check from [[SemanticCheckContext]]. */
  def fromContext(f: SemanticCheckContext => SemanticCheck): SemanticCheck = CheckFromContext(f)

  /** Creates a check which uses both the current [[SemanticState]] and [[SemanticCheckContext]]. */
  def fromFunctionWithContext(f: (SemanticState, SemanticCheckContext) => SemanticCheckResult): SemanticCheck = {
    fromContext(context => fromFunction(state => f(state, context)))
  }

  /** Creates the next check by lazily evaluating `check`.
   * 
   * This can be used to wrap recursive checks, as it lets [[SemanticCheckInterpreter]] evaluate only a single
   * iteration of such check at a time, thus making it stack-safe.
   */
  def nestedCheck(check: => SemanticCheck): SemanticCheck = success.flatMap(_ => check)

  /** Creates a check which runs `check` if `condition` is `true`, otherwise does nothing. */
  def when(condition: Boolean)(check: => SemanticCheck): SemanticCheck = {
    if (condition)
      check
    else
      SemanticCheck.success
  }

  private[semantics] val DEBUG_ENABLED = false

  final private[semantics] case class Leaf(f: SemanticState => SemanticCheckResult) extends SemanticCheck

  final private[semantics] case class Map(check: SemanticCheck, f: SemanticCheckResult => SemanticCheckResult)
      extends SemanticCheck

  final private[semantics] case class FlatMap(check: SemanticCheck, f: SemanticCheckResult => SemanticCheck)
      extends SemanticCheck

  final private[semantics] case class CheckFromContext(f: SemanticCheckContext => SemanticCheck) extends SemanticCheck
  final private[semantics] case class Annotated(check: SemanticCheck, annotation: String) extends SemanticCheck
}

final case class SemanticCheckResult(state: SemanticState, errors: Seq[SemanticErrorDef])

object SemanticCheckResult {
  def success(s: SemanticState): SemanticCheckResult = SemanticCheckResult(s, Vector.empty)

  def error(state: SemanticState, error: SemanticErrorDef): SemanticCheckResult =
    SemanticCheckResult(state, Vector(error))

  def error(state: SemanticState, msg: String, position: InputPosition): SemanticCheckResult =
    error(state, SemanticError(msg, position))

  def error(state: SemanticState, error: Option[SemanticErrorDef]): SemanticCheckResult =
    SemanticCheckResult(state, error.toVector)
}

trait SemanticCheckContext {
  def errorMessageProvider: ErrorMessageProvider
}

object SemanticCheckContext {

  object default extends SemanticCheckContext {
    override def errorMessageProvider: ErrorMessageProvider = NotImplementedErrorMessageProvider
  }
}

class OptionSemanticChecking[A](val option: Option[A]) extends AnyVal {

  def foldSemanticCheck(check: A => SemanticCheck): SemanticCheck =
    option.fold(SemanticCheck.success)(check)
}

class TraversableOnceSemanticChecking[A](val traversable: IterableOnce[A]) extends AnyVal {

  def foldSemanticCheck(check: A => SemanticCheck): SemanticCheck = {
    traversable.iterator.foldLeft(SemanticCheck.success) {
      (accCheck, o) => accCheck chain check(o)
    }
  }
}

trait SemanticCheckable {
  def semanticCheck: SemanticCheck
}

trait SemanticCheckableExpression {
  def semanticCheck(ctx: SemanticContext): SemanticCheck
}

class SemanticCheckableOption[A <: SemanticCheckable](val option: Option[A]) extends AnyVal {
  def semanticCheck: SemanticCheck = option.fold(SemanticCheck.success) { _.semanticCheck }
}

class SemanticCheckableTraversableOnce[A <: SemanticCheckable](val traversable: IterableOnce[A]) extends AnyVal {
  def semanticCheck: SemanticCheck = traversable.foldSemanticCheck { _.semanticCheck }
}
