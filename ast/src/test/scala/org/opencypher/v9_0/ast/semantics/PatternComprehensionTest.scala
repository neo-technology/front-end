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

import org.opencypher.v9_0.expressions
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.Property
import org.opencypher.v9_0.expressions.PropertyKeyName
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.RelationshipsPattern
import org.opencypher.v9_0.expressions.SemanticDirection
import org.opencypher.v9_0.expressions.StringLiteral
import org.opencypher.v9_0.util.symbols.CTBoolean
import org.opencypher.v9_0.util.symbols.CTList
import org.opencypher.v9_0.util.symbols.CTString
import org.opencypher.v9_0.util.symbols.StorableType

class PatternComprehensionTest extends SemanticFunSuite {

  val n = NodePattern(Some(variable("n")), None, None, None)(pos)
  val x = expressions.NodePattern(Some(variable("x")), None, None, None)(pos)
  val r = RelationshipPattern(None, Seq.empty, None, None, None, SemanticDirection.OUTGOING)(pos)
  val pattern = RelationshipsPattern(RelationshipChain(n, r, x)(pos))(pos)
  val property = Property(variable("x"), PropertyKeyName("prop")(pos))(pos)
  val failingProperty = Property(variable("missing"), PropertyKeyName("prop")(pos))(pos)
  val stringLiteral = StringLiteral("APA")(pos)

  test("pattern comprehension on a property returns the expected type") {
    val expression = PatternComprehension(None, pattern, None, property)(pos, Set.empty, "", "")

    val result = SemanticExpressionCheck.simple(expression)(SemanticState.clean)

    result.errors shouldBe empty
    types(expression)(result.state) should equal(StorableType.storableType.wrapInList)
  }

  test("pattern comprehension with literal string projection has correct type") {
    val expression = PatternComprehension(None, pattern, None, stringLiteral)(pos, Set.empty, "", "")

    val result = SemanticExpressionCheck.simple(expression)(SemanticState.clean)

    result.errors shouldBe empty
    types(expression)(result.state) should equal(CTList(CTString).invariant)
  }

  test("inner projection using missing identifier reports error") {
    val expression = PatternComprehension(None, pattern, None, failingProperty)(pos, Set.empty, "", "")

    val result = SemanticExpressionCheck.simple(expression)(SemanticState.clean)

    result.errors shouldBe Seq(SemanticError("Variable `missing` not defined", pos))
  }

  test("inner filter using missing identifier reports error") {
    val expression = PatternComprehension(None, pattern, Some(failingProperty), stringLiteral)(pos, Set.empty, "", "")

    val result = SemanticExpressionCheck.simple(expression)(SemanticState.clean)

    result.errors shouldBe Seq(SemanticError("Variable `missing` not defined", pos))
  }

  test("pattern can't reuse identifier with different type") {
    val expression = PatternComprehension(None, pattern, None, stringLiteral)(pos, Set.empty, "", "")

    val semanticState = SemanticState.clean.declareVariable(variable("n"), CTBoolean).right.get
    val result = SemanticExpressionCheck.simple(expression)(semanticState)

    result.errors shouldBe Seq(
      SemanticError("Type mismatch: n defined with conflicting type Boolean (expected Node)", pos)
    )
  }
}
