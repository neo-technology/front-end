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
package org.opencypher.v9_0.rewriting.rewriters

import org.opencypher.v9_0.expressions.FunctionInvocation
import org.opencypher.v9_0.expressions.FunctionName
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.rewriting.rewriters.factories.PreparatoryRewritingRewriterFactory
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.InternalNotificationLogger
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Condition
import org.opencypher.v9_0.util.StepSequencer.Step
import org.opencypher.v9_0.util.topDown

case object TimestampRewritten extends Condition

case object timestampRewriter extends Rewriter with Step with PreparatoryRewritingRewriterFactory {

  override def getRewriter(
    cypherExceptionFactory: CypherExceptionFactory,
    notificationLogger: InternalNotificationLogger
  ): Rewriter = instance

  override def preConditions: Set[StepSequencer.Condition] = Set.empty

  override def postConditions: Set[StepSequencer.Condition] = Set(TimestampRewritten)

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set.empty

  def apply(that: AnyRef): AnyRef = instance(that)

  private val rewriter = Rewriter.lift {

    case f @ FunctionInvocation(namespace, FunctionName(name), _, _)
      if namespace.parts.isEmpty && name.equalsIgnoreCase("timestamp") =>
      val datetimeFunction = f.copy(functionName = FunctionName("datetime")(f.functionName.position))(f.position)
      Property(datetimeFunction, PropertyKeyName("epochMillis")(datetimeFunction.position))(datetimeFunction.position)
  }

  private val instance = topDown(rewriter)
}
