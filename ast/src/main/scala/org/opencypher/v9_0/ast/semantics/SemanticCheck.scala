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
import org.opencypher.v9_0.util.InputPosition

sealed trait SemanticCheck {

  @deprecated(message = "Use `run` instead", since = "5.0")
  def apply(state: SemanticState): SemanticCheckResult = {
    run(state)
  }

  def run(state: SemanticState): SemanticCheckResult = {
    SemanticCheckInterpreter.runCheck(this, state)
  }

  def chain(next: SemanticCheck): SemanticCheck = {
    for {
      a <- this
      b <- next
    } yield SemanticCheckResult(b.state, a.errors ++ b.errors)
  }

  def ifOkChain(next: => SemanticCheck): SemanticCheck = {
    for {
      a <- this
      b <- when(a.errors.isEmpty)(next)
    } yield SemanticCheckResult(b.state, a.errors ++ b.errors)
  }

  def map(f: SemanticCheckResult => SemanticCheckResult): SemanticCheck = SemanticCheck.Map(this, f)
  def flatMap(f: SemanticCheckResult => SemanticCheck): SemanticCheck = SemanticCheck.FlatMap(this, f)
}

object SemanticCheck {

  val success: SemanticCheck = fromFunction(SemanticCheckResult.success)
  def error(error: SemanticErrorDef): SemanticCheck = fromFunction(SemanticCheckResult.error(_, error))

  def getState: SemanticCheck = success
  def setState(s: SemanticState): SemanticCheck = fromFunction(_ => SemanticCheckResult.success(s))

  def fromFunction(f: SemanticState => SemanticCheckResult): SemanticCheck = Leaf(f)
  def fromState(f: SemanticState => SemanticCheck): SemanticCheck = success.flatMap(res => f(res.state))

  def nestedCheck(check: => SemanticCheck): SemanticCheck = success.flatMap(_ => check)

  def when(condition: Boolean)(check: => SemanticCheck): SemanticCheck = {
    if (condition)
      check
    else
      SemanticCheck.success
  }

  final private[semantics] case class Leaf(f: SemanticState => SemanticCheckResult) extends SemanticCheck

  final private[semantics] case class Map(check: SemanticCheck, f: SemanticCheckResult => SemanticCheckResult)
      extends SemanticCheck

  final private[semantics] case class FlatMap(check: SemanticCheck, f: SemanticCheckResult => SemanticCheck)
      extends SemanticCheck
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
