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
package org.opencypher.v9_0.frontend.helpers

import org.opencypher.v9_0.ast.semantics.SemanticErrorDef
import org.opencypher.v9_0.frontend.PlannerName
import org.opencypher.v9_0.frontend.phases.BaseContext
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer
import org.opencypher.v9_0.frontend.phases.Monitors
import org.opencypher.v9_0.util.CancellationChecker
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.ErrorMessageProvider
import org.opencypher.v9_0.util.NotImplementedErrorMessageProvider
import org.opencypher.v9_0.util.OpenCypherExceptionFactory
import org.opencypher.v9_0.util.devNullLogger
import org.scalatest.matchers.MatchResult
import org.scalatest.matchers.Matcher

class ErrorCollectingContext extends BaseContext {

  var errors: Seq[SemanticErrorDef] = Seq.empty

  override def tracer: CompilationPhaseTracer = CompilationPhaseTracer.NO_TRACING
  override def notificationLogger: devNullLogger.type = devNullLogger
  override def cypherExceptionFactory: CypherExceptionFactory = OpenCypherExceptionFactory(None)
  override def monitors: Monitors = ???

  override def errorHandler: Seq[SemanticErrorDef] => Unit = (errs: Seq[SemanticErrorDef]) =>
    errors = errs

  override def errorMessageProvider: ErrorMessageProvider = NotImplementedErrorMessageProvider

  override def cancellationChecker: CancellationChecker = CancellationChecker.NeverCancelled
}

object ErrorCollectingContext {

  def failWith(errorMessages: String*): Matcher[ErrorCollectingContext] = new Matcher[ErrorCollectingContext] {

    override def apply(context: ErrorCollectingContext): MatchResult = {
      MatchResult(
        matches = context.errors.map(_.msg) == errorMessages,
        rawFailureMessage = s"Expected errors: $errorMessages but got ${context.errors}",
        rawNegatedFailureMessage = s"Did not expect errors: $errorMessages."
      )
    }
  }
}

object NoPlannerName extends PlannerName {
  override def name = "no planner"
  override def toTextOutput = "no planner"
  override def version = "no version"
}
