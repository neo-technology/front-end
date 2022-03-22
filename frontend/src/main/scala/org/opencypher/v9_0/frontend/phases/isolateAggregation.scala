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

import org.opencypher.v9_0.ast.AliasedReturnItem
import org.opencypher.v9_0.ast.Clause
import org.opencypher.v9_0.ast.ProjectionClause
import org.opencypher.v9_0.ast.ReturnItem
import org.opencypher.v9_0.ast.ReturnItems
import org.opencypher.v9_0.ast.SingleQuery
import org.opencypher.v9_0.ast.UnaliasedReturnItem
import org.opencypher.v9_0.ast.With
import org.opencypher.v9_0.ast.semantics.SemanticFeature
import org.opencypher.v9_0.expressions.DesugaredMapProjection
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.IsAggregate
import org.opencypher.v9_0.expressions.IterablePredicateExpression
import org.opencypher.v9_0.expressions.ListComprehension
import org.opencypher.v9_0.expressions.ReduceExpression
import org.opencypher.v9_0.expressions.Variable
import org.opencypher.v9_0.frontend.phases.factories.PlanPipelineTransformerFactory
import org.opencypher.v9_0.rewriting.conditions.SemanticInfoAvailable
import org.opencypher.v9_0.rewriting.conditions.aggregationsAreIsolated
import org.opencypher.v9_0.rewriting.conditions.hasAggregateButIsNotAggregate
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.bottomUp
import org.opencypher.v9_0.util.helpers.fixedPoint
import org.opencypher.v9_0.util.topDown

/**
 * This rewriter makes sure that aggregations are on their own in RETURN/WITH clauses, so
 * the planner can have an easy time
 *
 * Example:
 *
 * MATCH (n)
 * RETURN { name: n.name, count: count(*) }, n.foo
 *
 * This query has a RETURN clause where the single expression contains both the aggregate key and
 * the aggregation expression. To make the job easier on the planner, this rewrite will change the query to:
 *
 * MATCH (n)
 * WITH n.name AS x1, count(*) AS x2, n.foo as X3
 * RETURN { name: x1, count: x2 }
 */
case object isolateAggregation extends StatementRewriter with StepSequencer.Step with PlanPipelineTransformerFactory {

  override def instance(from: BaseState, context: BaseContext): Rewriter = bottomUp(rewriter(from), _.isInstanceOf[Expression], context.cancellationChecker)

  private def rewriter(from: BaseState) = Rewriter.lift {
    case q@SingleQuery(clauses) =>

      val newClauses = clauses.flatMap {
        case clause: ProjectionClause if clauseNeedingWork(clause) =>
          val clauseReturnItems = clause.returnItems.items
          val (withAggregations, others) = clauseReturnItems.map(_.expression).toSet.partition(hasAggregateButIsNotAggregate(_))

          val expressionsToIncludeInWith: Set[Expression] = others ++ extractExpressionsToInclude(withAggregations)

          val withReturnItems: Set[ReturnItem] = expressionsToIncludeInWith.map {
            e =>
              AliasedReturnItem(e, Variable(from.anonymousVariableNameGenerator.nextName)(e.position))(e.position, isAutoAliased = true)
          }
          val pos = clause.position
          val withClause = With(distinct = false, ReturnItems(includeExisting = false, withReturnItems.toIndexedSeq)(pos), None, None, None, None)(pos)

          val expressionRewriter = createRewriterFor(withReturnItems)
          val newReturnItems = clauseReturnItems.map {
            case ri@AliasedReturnItem(expression, _) => ri.copy(expression = expression.endoRewrite(expressionRewriter))(ri.position, ri.isAutoAliased)
            case ri@UnaliasedReturnItem(expression, _) => ri.copy(expression = expression.endoRewrite(expressionRewriter))(ri.position)
          }
          val resultClause = clause.withReturnItems(newReturnItems)

          IndexedSeq(withClause, resultClause)

        case clause => IndexedSeq(clause)
      }

      q.copy(clauses = newClauses)(q.position)
  }

  private def createRewriterFor(withReturnItems: Set[ReturnItem]): Rewriter = {
    def inner = Rewriter.lift {
      case original: Expression =>
        val rewrittenExpression = withReturnItems.collectFirst {
          case item@AliasedReturnItem(expression, _) if original == expression =>
            item.alias.get.copyId
        }
        rewrittenExpression getOrElse original
    }
    topDown(inner)
  }

  private def extractExpressionsToInclude(originalExpressions: Set[Expression]): Set[Expression] = {
    val expressionsToGoToWith: Set[Expression] = fixedPoint {
      expressions: Set[Expression] => expressions.flatMap {
        case e@ReduceExpression(_, init, coll) if hasAggregateButIsNotAggregate(e) =>
          Seq(init, coll)

        case e@ListComprehension(_, expr) if hasAggregateButIsNotAggregate(e) =>
          Seq(expr)

        case e@DesugaredMapProjection(variable, items, _) if hasAggregateButIsNotAggregate(e) =>
          items.map(_.exp) :+ variable

        case e: IterablePredicateExpression  if hasAggregateButIsNotAggregate(e) =>
          val predicate: Expression = e.innerPredicate.getOrElse(throw new IllegalStateException("Should never be empty"))
          // Weird way of doing it to make scalac happy
          Set(e.expression) ++ predicate.dependencies - e.variable

        case e if hasAggregateButIsNotAggregate(e) =>
          e.arguments

        case e =>
          Seq(e)
      }
    }(originalExpressions).filter {
      //Constant expressions should never be isolated
      expr => IsAggregate(expr) || expr.dependencies.nonEmpty
    }
    expressionsToGoToWith
  }

  private def clauseNeedingWork(c: Clause): Boolean = c.treeExists {
    case e: Expression => hasAggregateButIsNotAggregate(e)
  }

  override def preConditions: Set[StepSequencer.Condition] = Set(
    // Otherwise it might rewrite ambiguous symbols incorrectly, e.g. when a grouping variable is shadowed in for-comprehension.
    AmbiguousNamesDisambiguated
  )

  override def postConditions: Set[StepSequencer.Condition] = Set(StatementCondition(aggregationsAreIsolated))

  override def invalidatedConditions: Set[StepSequencer.Condition] = Set(
    // Can introduces new ambiguous variable names itself.
    AmbiguousNamesDisambiguated,
  ) ++ SemanticInfoAvailable // Adds a WITH clause with no SemanticInfo

  override def getTransformer(pushdownPropertyReads: Boolean,
                              semanticFeatures: Seq[SemanticFeature]): Transformer[BaseContext, BaseState, BaseState] = this

}
