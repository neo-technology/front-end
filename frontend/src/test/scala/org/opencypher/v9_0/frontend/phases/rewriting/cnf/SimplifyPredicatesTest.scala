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

import org.opencypher.v9_0.ast.UnaliasedReturnItem
import org.opencypher.v9_0.ast.factory.neo4j.JavaCCParser
import org.opencypher.v9_0.ast.semantics.SemanticState
import org.opencypher.v9_0.expressions.Ands
import org.opencypher.v9_0.expressions.AutoExtractedParameter
import org.opencypher.v9_0.expressions.Equals
import org.opencypher.v9_0.expressions.ExplicitParameter
import org.opencypher.v9_0.expressions.False
import org.opencypher.v9_0.expressions.IsNotNull
import org.opencypher.v9_0.expressions.IsNull
import org.opencypher.v9_0.expressions.Not
import org.opencypher.v9_0.expressions.Null
import org.opencypher.v9_0.expressions.Ors
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.SignedDecimalIntegerLiteral
import org.opencypher.v9_0.expressions.StringLiteral
import org.opencypher.v9_0.expressions.True
import org.opencypher.v9_0.logical.plans.CoerceToPredicate
import org.opencypher.v9_0.util.AnonymousVariableNameGenerator
import org.opencypher.v9_0.util.InputPosition
import org.opencypher.v9_0.util.OpenCypherExceptionFactory
import org.opencypher.v9_0.util.symbols.CTAny
import org.opencypher.v9_0.util.symbols.CTInteger
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite
import org.opencypher.v9_0.util.test_helpers.Extractors.SetExtractor

class SimplifyPredicatesTest extends CypherFunSuite {

  test("double negation is removed by keeping an extra not") {
    // not(not(not(P))) <=> not(P)
    assertRewrittenMatches("NOT NOT NOT 'P'", { case Not(StringLiteral("P")) => () })
  }

  test("repeated double negation is removed") {
    // not(not(not(not(P)))) <=> bool(P)
    assertRewrittenMatches("NOT NOT NOT NOT 'P'", { case CoerceToPredicate(StringLiteral("P")) => () })
  }

  test("double negation is removed") {
    // not(not(P)) <=> bool(P)
    assertRewrittenMatches("NOT NOT 'P'", { case CoerceToPredicate(StringLiteral("P")) => () })

    // not(not(TRUE)) <=> TRUE
    assertRewrittenMatches("NOT NOT TRUE", { case True() => () })
  }

  test("double negation on pattern comprehension") {
    // NOT NOT ()--() -> bool(()--())
    assertRewrittenMatches("NOT NOT ()--()", { case CoerceToPredicate(PatternExpression(_)) => () })
  }

  test("double negation on null") {
    // NOT NOT null -> null
    assertRewrittenMatches("NOT NOT null", { case Null() => () })
  }

  test("OR + double negation") {
    // or(not(not(P)), not(not(Q))) <=> or(P, Q)
    assertRewrittenMatches(
      "NOT NOT 'P' OR NOT NOT 'Q'",
      { case Ors(SetExtractor(StringLiteral("P"), StringLiteral("Q"))) => () }
    )
  }

  test("NOT IS NULL is rewritten") {
    // not(isNull(P)) <=> isNotNull(P)
    assertRewrittenMatches("NOT( 'P' IS NULL )", { case IsNotNull(StringLiteral("P")) => () })
  }

  test("NOT IS NOT NULL is rewritten") {
    // not(isNotNull(P)) <=> isNull(P)
    assertRewrittenMatches("NOT( 'P' IS NOT NULL )", { case IsNull(StringLiteral("P")) => () })
  }

  test("Simplify OR of identical expressions with interspersed condition") {
    // We should be able to remove one of those redundant $n = 2.
    assertRewrittenMatches("$n = 2 OR $n = 1 OR $n = 2", { case Ors(SetExtractor(Equals(_, _), Equals(_, _))) => () })
  }

  test("Simplify negated false") {
    assertRewrittenMatches("$n.a OR NOT false", { case True() => () })
  }

  test("Simplify negated true") {
    assertRewrittenMatches("$n.a AND NOT true", { case False() => () })
  }

  test("Do not simplify expressions with different auto extracted parameters") {
    val position = InputPosition(0, 0, 0)
    // AST for $n = 2 OR $n = 3
    val ast = Ors(Seq(
      Equals(
        ExplicitParameter("n", CTAny)(position),
        AutoExtractedParameter("AUTOINT0", CTInteger, SignedDecimalIntegerLiteral("2")(position))(position)
      )(position),
      Equals(
        ExplicitParameter("n", CTAny)(position),
        AutoExtractedParameter("AUTOINT1", CTInteger, SignedDecimalIntegerLiteral("3")(position))(position)
      )(position)
    ))(position)
    val rewriter = flattenBooleanOperators andThen simplifyPredicates(SemanticState.clean)
    val result = ast.rewrite(rewriter)
    ast should equal(result)
  }

  test("should not simplify self-negation") {
    // because in ternary logic NULL AND not NULL = NULL, we cannot simplify this to false, as one might be tempted to do
    assertRewrittenMatches("$n.a AND NOT $n.a", { case Ands(_) => () })
  }

  private val exceptionFactory = new OpenCypherExceptionFactory(None)

  private def assertRewrittenMatches(originalQuery: String, matcher: PartialFunction[Any, Unit]): Unit = {
    val original = JavaCCParser.parse("RETURN " + originalQuery, exceptionFactory, new AnonymousVariableNameGenerator())
    val checkResult = original.semanticCheck(SemanticState.clean)
    val rewriter = flattenBooleanOperators andThen simplifyPredicates(checkResult.state)
    val result = original.endoRewrite(rewriter)
    val maybeReturnExp = result.folder.treeFind({
      case UnaliasedReturnItem(expression, _) => {
        assert(matcher.isDefinedAt(expression), expression)
        true
      }
    }: PartialFunction[AnyRef, Boolean])
    assert(maybeReturnExp.isDefined, "Could not find return in parsed query!")
  }
}
