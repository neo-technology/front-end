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

import org.opencypher.v9_0.ast.UnaliasedReturnItem
import org.opencypher.v9_0.ast.semantics.SemanticCheckResult
import org.opencypher.v9_0.ast.semantics.SemanticChecker
import org.opencypher.v9_0.ast.semantics.SemanticFeature
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.ast.semantics.SemanticTable
import org.opencypher.v9_0.frontend.phases.CompilationPhaseTracer.CompilationPhase.SEMANTIC_CHECK
import org.opencypher.v9_0.frontend.phases.factories.PlanPipelineTransformerFactory
import org.opencypher.v9_0.rewriting.conditions.SemanticInfoAvailable
import org.opencypher.v9_0.rewriting.conditions.StateContainsSemanticTable
import org.opencypher.v9_0.rewriting.conditions.containsNoNodesOfType
import org.opencypher.v9_0.rewriting.rewriters.recordScopes
import org.opencypher.v9_0.util.StepSequencer

case object TokensResolved extends StepSequencer.Condition

/**
 * Do variable binding, typing, type checking and other semantic checks.
 */
case class SemanticAnalysis(warn: Boolean, features: SemanticFeature*)
  extends Phase[BaseContext, BaseState, BaseState] {

  override def process(from: BaseState, context: BaseContext): BaseState = {
    val startState = SemanticState.clean.withFeatures(features: _*).withErrorMessageProvider(context.errorMessageProvider)

    val SemanticCheckResult(state, errors) = SemanticChecker.check(from.statement(), startState)
    if (warn) state.notifications.foreach(context.notificationLogger.log)

    context.errorHandler(errors)

    val table = from.maybeSemanticTable match {
      case Some(existingTable) =>
      // We might already have a SemanticTable from a previous run, and that might already have tokens.
      // We don't want to lose these
        existingTable.copy(types = state.typeTable, recordedScopes = state.recordedScopes.view.mapValues(_.scope).toMap)
      case None => SemanticTable(types = state.typeTable, recordedScopes = state.recordedScopes.view.mapValues(_.scope).toMap)
    }

    val rewrittenStatement = if (errors.isEmpty) {
      // Some expressions record some semantic information in themselves.
      // This is done by the recordScopes rewriter.
      // We need to apply it after each pass of SemanticAnalysis.
      from.statement().endoRewrite(recordScopes(state))
    } else {
      // If we have errors we should rather avoid running recordScopes, since the state might be incomplete.
      from.statement()
    }
    from
      .withStatement(rewrittenStatement)
      .withSemanticState(state)
      .withSemanticTable(table)
  }

  override def phase: CompilationPhaseTracer.CompilationPhase = SEMANTIC_CHECK

  override def postConditions = Set(BaseContains[SemanticState], StatementCondition(containsNoNodesOfType[UnaliasedReturnItem]))
}

case object SemanticAnalysis extends StepSequencer.Step with PlanPipelineTransformerFactory {
  override def preConditions: Set[StepSequencer.Condition] = Set.empty

  override def postConditions: Set[StepSequencer.Condition] = Set(StateContainsSemanticTable) ++ SemanticInfoAvailable

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set.empty

  override def getTransformer(pushdownPropertyReads: Boolean,
                              semanticFeatures: Seq[SemanticFeature]): Transformer[BaseContext, BaseState, BaseState] = SemanticAnalysis(warn = false, semanticFeatures: _*)
}