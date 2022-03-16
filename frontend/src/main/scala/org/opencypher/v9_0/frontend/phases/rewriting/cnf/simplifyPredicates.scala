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
package org.opencypher.v9_0.frontend.phases.rewriting.cnf

import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.AllIterablePredicate
import org.opencypher.v9_0.expressions.Ands
import org.opencypher.v9_0.expressions.BooleanExpression
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.False
import org.opencypher.v9_0.expressions.FilterScope
import org.opencypher.v9_0.expressions.IsNotNull
import org.opencypher.v9_0.expressions.IsNull
import org.opencypher.v9_0.expressions.Not
import org.opencypher.v9_0.expressions.Ors
import org.opencypher.v9_0.expressions.True
import org.opencypher.v9_0.frontend.phases.BaseContext
import org.opencypher.v9_0.frontend.phases.BaseState
import org.opencypher.v9_0.frontend.phases.factories.PlanPipelineTransformerFactory
import org.opencypher.v9_0.frontend.phases.rewriting.cnf.simplifyPredicates.coerceInnerExpressionToBooleanIfNecessary
import org.opencypher.v9_0.logical.plans.CoerceToPredicate
import org.opencypher.v9_0.rewriting.conditions.SemanticInfoAvailable
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.helpers.fixedPoint
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.topDown

case class simplifyPredicates(semanticState: SemanticState) extends Rewriter {
  private val T = True()(null)
  private val F = False()(null)

  private val step: Rewriter = Rewriter.lift { case e: Expression => computeReplacement(e) }

  private val instance = fixedPoint(topDown(step))

  def apply(that: AnyRef): AnyRef = {
    instance.apply(that)
  }

  private def computeReplacement: Expression => Expression = {
    case n@Not(Not(innerExpression)) => simplifyToInnerExpression(n, innerExpression)
    case n@Not(IsNull(innerExpression)) => IsNotNull(innerExpression)(n.position)
    case Ands(exps)   if exps.isEmpty => throw new IllegalStateException("Found an instance of Ands with empty expressions")
    case Ors(exps)    if exps.isEmpty => throw new IllegalStateException("Found an instance of Ors with empty expressions")
    case p@Ands(exps) if exps.contains(F) => False()(p.position)
    case p@Ors(exps)  if exps.contains(T) => True()(p.position)
    case p@Ands(exps) if exps.size == 1   => simplifyToInnerExpression(p, exps.head)
    case p@Ors(exps)  if exps.size == 1   => simplifyToInnerExpression(p, exps.head)
    case p@Ands(exps) if exps.contains(T) =>
      val nonTrue = exps.filterNot(T == _)
      if (nonTrue.isEmpty)
        True()(p.position)
      else if(nonTrue.size == 1)
        simplifyToInnerExpression(p, nonTrue.head)
      else
        Ands(nonTrue)(p.position)
    case p@Ors(exps) if exps.contains(F) =>
      val nonFalse = exps.filterNot(F == _)
      if (nonFalse.isEmpty)
        False()(p.position)
      else if(nonFalse.size == 1)
        simplifyToInnerExpression(p, nonFalse.head)
      else
        Ors(nonFalse)(p.position)

    // technically, this is not simplification but it helps addressing the separate predicates in the conjunction
    case all@AllIterablePredicate(fs@FilterScope(variable, Some(Ands(preds))), expression) =>
      val predicates = preds.map { predicate =>
        AllIterablePredicate(FilterScope(variable, Some(predicate))(fs.position), expression)(all.position)
      }
      Ands(predicates)(all.position)
    case expression => expression
  }

  private def simplifyToInnerExpression(outerExpression: BooleanExpression,
                                        innerExpression: Expression) = {
    val newExpression = computeReplacement(innerExpression)
    coerceInnerExpressionToBooleanIfNecessary(semanticState, outerExpression, newExpression)
  }
}

case object simplifyPredicates extends StepSequencer.Step with PlanPipelineTransformerFactory with CnfPhase {

  override def preConditions: Set[StepSequencer.Condition] = Set(AndRewrittenToAnds) ++ SemanticInfoAvailable

  override def postConditions: Set[StepSequencer.Condition] = Set(PredicatesSimplified)

  override def invalidatedConditions: Set[StepSequencer.Condition] = SemanticInfoAvailable

  override def getRewriter(from: BaseState,
                           context: BaseContext): Rewriter = this (from.semantics())

  def coerceInnerExpressionToBooleanIfNecessary(semanticState: SemanticState,
                                                outerExpression: BooleanExpression,
                                                innerExpression: Expression): Expression = {
    if (needsToBeExplicitlyCoercedToBoolean(semanticState, outerExpression, innerExpression)) {
      CoerceToPredicate(innerExpression)
    } else {
      innerExpression
    }
  }

  /**
   * We intend to remove `outerExpression` from the AST and replace it with `innerExpression`.
   *
   * While `outerExpression` would have converted the value to boolean, we check here whether that information would be lost.
   */
  private def needsToBeExplicitlyCoercedToBoolean(semanticState: SemanticState, outerExpression: BooleanExpression, innerExpression: Expression) = {
    val expectedToBeBoolean = semanticState.expressionType(outerExpression).expected.exists(_.contains(CTBoolean))
    val specifiedToBeBoolean = semanticState.expressionType(innerExpression).specified.contains(CTBoolean)
    !expectedToBeBoolean && !specifiedToBeBoolean
  }
}
