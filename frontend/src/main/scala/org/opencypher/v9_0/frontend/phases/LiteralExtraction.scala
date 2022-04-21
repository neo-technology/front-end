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
package org.opencypher.v9_0.frontend.phases

import org.opencypher.v9_0.ast.AdministrationCommand
import org.opencypher.v9_0.ast.SchemaCommand
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.AST_REWRITE
import org.opencypher.v9_0.rewriting.rewriters.LiteralExtractionStrategy
import org.opencypher.v9_0.rewriting.rewriters.LiteralsAreAvailable
import org.opencypher.v9_0.rewriting.rewriters.literalReplacement
import org.opencypher.v9_0.rewriting.rewriters.sensitiveLiteralReplacement
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Step

/**
 * Replace literals with parameters.
 */
case class LiteralExtraction(literalExtraction: LiteralExtractionStrategy)
    extends Phase[BaseContext, BaseState, BaseState] with Step {

  override def process(in: BaseState, context: BaseContext): BaseState = {
    val statement = in.statement()
    val (extractParameters, extractedParameters) = statement match {
      case _: AdministrationCommand => sensitiveLiteralReplacement(statement)
      case _: SchemaCommand         => Rewriter.noop -> Map.empty[String, Any]
      case _                        => literalReplacement(statement, literalExtraction)
    }
    val rewrittenStatement = statement.endoRewrite(extractParameters)
    in.withStatement(rewrittenStatement).withParams(extractedParameters)
  }

  override def phase = AST_REWRITE

  override def preConditions: Set[StepSequencer.Condition] = Set.empty

  override def postConditions: Set[StepSequencer.Condition] = Set.empty

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set(LiteralsAreAvailable)
}
