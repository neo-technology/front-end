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

import org.opencypher.v9_0.ast.AliasedReturnItem
import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.Return
import org.opencypher.v9_0.ast.ReturnItem
import org.opencypher.v9_0.ast.ReturnItems
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.Yield
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.rewriting.conditions.containsNoReturnAll
import org.opencypher.v9_0.rewriting.rewriters.factories.ASTRewriterFactory
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Condition
import org.opencypher.v9_0.util.bottomUp
import org.opencypher.v9_0.util.symbols.CypherType

case object ProjectionClausesHaveSemanticInfo extends Condition

case class expandStar(state: SemanticState) extends Rewriter {

  override def apply(that: AnyRef): AnyRef = instance(that)

  private val rewriter = Rewriter.lift {
    case clause @ With(_, values, _, _, _, _) if values.includeExisting =>
      val newReturnItems =
        if (values.includeExisting) returnItems(clause, values.items, values.defaultOrderOnColumns) else values
      clause.copy(returnItems = newReturnItems)(clause.position)

    case clause @ Return(_, values, _, _, _, excludedNames) if values.includeExisting =>
      val newReturnItems =
        if (values.includeExisting) returnItems(clause, values.items, values.defaultOrderOnColumns, excludedNames)
        else values
      clause.copy(returnItems = newReturnItems, excludedNames = Set.empty)(clause.position)

    case clause @ Yield(values, _, _, _, _) if values.includeExisting =>
      val newReturnItems =
        if (values.includeExisting) returnItems(clause, values.items, values.defaultOrderOnColumns) else values
      clause.copy(returnItems = newReturnItems)(clause.position)

    case expandedAstNode =>
      expandedAstNode
  }

  private val instance = bottomUp(rewriter, _.isInstanceOf[Expression])

  private def returnItems(
    clause: Clause,
    listedItems: Seq[ReturnItem],
    defaultOrderOnColumns: Option[List[String]],
    excludedNames: Set[String] = Set.empty
  ): ReturnItems = {
    val scope = state.scope(clause).getOrElse {
      throw new IllegalStateException(s"${clause.name} should note its Scope in the SemanticState")
    }

    val clausePos = clause.position
    val symbolNames = scope.symbolNames -- excludedNames -- listedItems.map(returnItem => returnItem.name)
    val orderedSymbolNames = defaultOrderOnColumns.map(columns => {
      val newColumns = symbolNames -- columns
      val ordered = columns.filter(symbolNames.contains) ++ newColumns
      ordered.toIndexedSeq
    }).getOrElse(symbolNames.toIndexedSeq.sorted)
    val expandedItems = orderedSymbolNames.map { id =>
      // We use the position of the clause for variables in new return items.
      // If the position was one of previous declaration, that could destroy scoping.
      val expr = Variable(id)(clausePos)
      val alias = expr.copyId
      AliasedReturnItem(expr, alias)(clausePos, isAutoAliased = true)
    }

    val newItems = expandedItems ++ listedItems
    ReturnItems(includeExisting = false, newItems)(clausePos)
  }
}

object expandStar extends StepSequencer.Step with ASTRewriterFactory {

  override def preConditions: Set[StepSequencer.Condition] = Set(
    ProjectionClausesHaveSemanticInfo // Looks up recorded scopes of projection clauses.
  )

  override def postConditions: Set[StepSequencer.Condition] = Set(containsNoReturnAll)

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set.empty

  override def getRewriter(
    semanticState: SemanticState,
    parameterTypeMapping: Map[String, CypherType],
    cypherExceptionFactory: CypherExceptionFactory,
    anonymousVariableNameGenerator: AnonymousVariableNameGenerator
  ): Rewriter = expandStar(semanticState)
}
