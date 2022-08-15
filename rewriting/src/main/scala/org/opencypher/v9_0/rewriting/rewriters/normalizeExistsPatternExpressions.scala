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

import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.CountExpression
import org.opencypher.v9_0.expressions.Equals
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.FunctionInvocation
import org.opencypher.v9_0.expressions.GreaterThan
import org.opencypher.v9_0.expressions.LessThan
import org.opencypher.v9_0.expressions.Not
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipsPattern
import org.opencypher.v9_0.expressions.SignedDecimalIntegerLiteral
import org.opencypher.v9_0.expressions.functions.Exists
import org.opencypher.v9_0.expressions.functions.Size
import org.opencypher.v9_0.rewriting.conditions.PatternExpressionAreWrappedInExists
import org.opencypher.v9_0.rewriting.conditions.PatternExpressionsHaveSemanticInfo
import org.opencypher.v9_0.rewriting.rewriters.factories.ASTRewriterFactory
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.CypherExceptionFactory
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.StepSequencer
import org.opencypher.v9_0.util.StepSequencer.Condition
import org.opencypher.v9_0.util.bottomUp
import org.opencypher.v9_0.util.symbols
import org.opencypher.v9_0.util.symbols.CypherType

/**
 * Adds an exists() around any pattern expression that is expected to produce a boolean e.g.
 *
 * MATCH (n) WHERE (n)-->(m) RETURN n
 *
 * is rewritten to
 *
 * MATCH (n) WHERE EXISTS((n)-->(m)) RETURN n
 *
 * Rewrite equivalent expressions with `size` or `length` to `exists`.
 * This rewrite normalizes this cases and make it easier to plan correctly.
 *
 * [[simplifyPredicates]] takes care of rewriting the Not(Not(Exists(...))) which can be introduced by this rewriter.
 *
 * This rewriter needs to run before [[namePatternElements]], which rewrites pattern expressions. Otherwise we don't find them in the semantic table.
 */
case class normalizeExistsPatternExpressions(
  semanticState: SemanticState,
  anonymousVariableNameGenerator: AnonymousVariableNameGenerator
) extends Rewriter {

  private val CountConverter = CountLikeToExistsConverter(anonymousVariableNameGenerator)

  private val instance = bottomUp(Rewriter.lift {
    case p: PatternExpression if semanticState.expressionType(p).expected.contains(symbols.CTBoolean.invariant) =>
      Exists(p)(p.position)

    case GreaterThan(CountConverter(exists), SignedDecimalIntegerLiteral("0")) => exists
    case LessThan(SignedDecimalIntegerLiteral("0"), CountConverter(exists))    => exists
    case Equals(CountConverter(exists), SignedDecimalIntegerLiteral("0"))      => Not(exists)(exists.position)
    case Equals(SignedDecimalIntegerLiteral("0"), CountConverter(exists))      => Not(exists)(exists.position)
  })

  override def apply(v: AnyRef): AnyRef = instance(v)
}

case object normalizeExistsPatternExpressions extends StepSequencer.Step with ASTRewriterFactory {

  override def preConditions: Set[Condition] = Set(
    PatternExpressionsHaveSemanticInfo // Looks up type of pattern expressions
  )

  override def postConditions: Set[Condition] = Set(PatternExpressionAreWrappedInExists)

  // TODO capture the dependency with simplifyPredicates
  override def invalidatedConditions: Set[Condition] = Set(
    ProjectionClausesHaveSemanticInfo // It can invalidate this condition by rewriting things inside WITH/RETURN.
  )

  override def getRewriter(
    semanticState: SemanticState,
    parameterTypeMapping: Map[String, CypherType],
    cypherExceptionFactory: CypherExceptionFactory,
    anonymousVariableNameGenerator: AnonymousVariableNameGenerator
  ): Rewriter = normalizeExistsPatternExpressions(semanticState, anonymousVariableNameGenerator)
}

final case class CountLikeToExistsConverter(anonymousVariableNameGenerator: AnonymousVariableNameGenerator) {

  def unapply(expression: Expression): Option[FunctionInvocation] = expression match {

    // size((n)--(m))
    case Size(p: PatternExpression) =>
      Some(Exists(p)(p.position))

    // size([pt = (n)--(m) | pt])
    case Size(p @ PatternComprehension(maybePt, pattern, None, _)) if p.introducedVariables == maybePt.toSet =>
      val exists =
        Exists(PatternExpression(pattern)(p.outerScope))(p.position)
      Some(exists)

    // COUNT { (n)--(m) }
    case ce @ CountExpression(pattern: RelationshipChain, None) if ce.introducedVariables.isEmpty =>
      val relPattern = RelationshipsPattern(pattern)(pattern.position)
      val variableToCollectName = anonymousVariableNameGenerator.nextName
      val collectionName = anonymousVariableNameGenerator.nextName
      val exists =
        Exists(PatternExpression(relPattern)(ce.outerScope))(ce.position)
      Some(exists)

    case _ =>
      None
  }
}
