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

import org.opencypher.v9_0.ast.AstConstructionTestSupport
import org.opencypher.v9_0.rewriting.Deprecations.semanticallyDeprecatedFeatures
import org.opencypher.v9_0.rewriting.Deprecations.syntacticallyDeprecatedFeatures
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ReplaceDeprecatedCypherSyntaxTest extends CypherFunSuite with AstConstructionTestSupport with RewritePhaseTest {

  override def rewriterPhaseUnderTest: Transformer[BaseContext, BaseState, BaseState] =
    SyntaxDeprecationWarningsAndReplacements(syntacticallyDeprecatedFeatures) andThen
      PreparatoryRewriting andThen
      SemanticAnalysis(warn = true) andThen
      SyntaxDeprecationWarningsAndReplacements(semanticallyDeprecatedFeatures)

  override def astRewriteAndAnalyze: Boolean = false

  test("should rewrite legacy relationship type disjunction") {
    assertRewritten(
      "MATCH (a)-[:A|:B|:C]-() RETURN a",
      "MATCH (a)-[:A|B|C]-() RETURN a"
    )
  }

  test("should rewrite setting of relationship properties to use properties function in a Set Clause") {
    assertRewritten(
      "MATCH (g)-[r:KNOWS]->(k) SET g = r",
      "MATCH (g)-[r:KNOWS]->(k) SET g = properties(r)"
    )
  }

  test("should rewrite setting of node properties to use properties function in a Set Clause") {
    assertRewritten(
      "MATCH (g)-[r:KNOWS]->(k) SET g = k",
      "MATCH (g)-[r:KNOWS]->(k) SET g = properties(k)"
    )
  }

  test("should rewrite setting of relationship properties to use properties function in a Mutate Set Clause") {
    assertRewritten(
      "MATCH (g)-[r:KNOWS]->(k) SET g += r",
      "MATCH (g)-[r:KNOWS]->(k) SET g += properties(r)"
    )
  }

  test("should rewrite setting of node properties to use properties function in a Mutate Set Clause") {
    assertRewritten(
      "MATCH (g)-[r:KNOWS]->(k) SET g += k",
      "MATCH (g)-[r:KNOWS]->(k) SET g += properties(k)"
    )
  }
}
