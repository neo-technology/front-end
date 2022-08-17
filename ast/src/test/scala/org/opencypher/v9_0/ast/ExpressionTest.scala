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
package org.opencypher.v9_0.ast

import org.opencypher.v9_0.expressions.CountExpression
import org.opencypher.v9_0.expressions.EveryPath
import org.opencypher.v9_0.expressions.ExistsExpression
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.LogicalVariable
import org.opencypher.v9_0.expressions.NodePattern
import org.opencypher.v9_0.expressions.Pattern
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.PatternExpression
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.RelationshipsPattern
import org.opencypher.v9_0.expressions.SemanticDirection
import org.opencypher.v9_0.util.IdentityMap
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ExpressionTest extends CypherFunSuite with AstConstructionTestSupport {

  test("should compute dependencies of simple expressions") {
    varFor("a").dependencies should equal(Set(varFor("a")))
    literalInt(1).dependencies should equal(Set())
  }

  test("should compute dependencies of composite expressions") {
    add(varFor("a"), subtract(literalInt(1), varFor("b"))).dependencies should equal(Set(varFor("a"), varFor("b")))
  }

  test("should compute dependencies for filtering expressions") {
    // [x IN (n)-->(k) | head(nodes(x)) ]
    val pat: RelationshipsPattern = RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _
    val expr: Expression = listComprehension(
      varFor("x"),
      PatternExpression(pat)(Set(varFor("n"), varFor("k"))),
      None,
      Some(function("head", function("nodes", varFor("x"))))
    )

    expr.dependencies should equal(Set(varFor("n"), varFor("k")))
  }

  test("should compute dependencies for nested filtering expressions") {
    // [x IN (n)-->(k) | [y IN [1,2,3] | y] ]
    val pat: RelationshipsPattern = RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _
    val innerExpr: Expression = listComprehension(
      varFor("y"),
      listOfInt(1, 2, 3),
      None,
      Some(varFor("y"))
    )
    val expr: Expression = listComprehension(
      varFor("x"),
      PatternExpression(pat)(Set(varFor("n"), varFor("k"))),
      None,
      Some(innerExpr)
    )

    expr.dependencies should equal(Set(varFor("n"), varFor("k")))
  }

  test("should compute dependencies for pattern comprehensions") {
    // [ (n)-->(k) | k ]
    val pat: RelationshipsPattern = RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _
    val expr = PatternComprehension(
      namedPath = None,
      pattern = pat,
      predicate = None,
      projection = varFor("k")
    )(pos, Set.empty)

    expr.withOuterScope(Set(varFor("n"), varFor("k"))).dependencies should equal(Set(varFor("n"), varFor("k")))
    expr.withOuterScope(Set.empty).dependencies should equal(Set.empty)
  }

  test("should compute dependencies for pattern comprehension with dependency in projection") {
    // [ (n)-->(k) | dep ]
    val pat: RelationshipsPattern = RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _
    val expr = PatternComprehension(
      namedPath = None,
      pattern = pat,
      predicate = None,
      projection = varFor("dep")
    )(pos, Set.empty)

    expr.withOuterScope(Set(varFor("n"), varFor("k"), varFor("dep"))).dependencies should equal(Set(
      varFor("n"),
      varFor("k"),
      varFor("dep")
    ))
  }

  test("should compute dependencies for pattern comprehension with dependency in predicate") {
    // [ (n)-->(k) WHERE dep | k ]
    val pat: RelationshipsPattern = RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _
    val expr = PatternComprehension(
      namedPath = None,
      pattern = pat,
      predicate = Some(varFor("dep")),
      projection = varFor("k")
    )(pos, Set.empty)

    expr.withOuterScope(Set(varFor("n"), varFor("k"), varFor("dep"))).dependencies should equal(Set(
      varFor("n"),
      varFor("k"),
      varFor("dep")
    ))
  }

  test("should compute dependencies for exists expression with node predicate") {
    // MATCH (n) WHERE EXISTS { (n)-[r]->(p) WHERE n.prop = p.prop }
    val relChain = RelationshipChain(
      NodePattern(Some(varFor("n")), None, None, None) _,
      RelationshipPattern(Some(varFor("r")), None, None, None, None, SemanticDirection.OUTGOING) _,
      NodePattern(Some(varFor("p")), None, None, None) _
    ) _

    val pattern = Pattern(Seq(EveryPath(relChain))) _

    val where = equals(prop(varFor("n"), "prop"), prop(varFor("p"), "prop"))

    val expr = ExistsExpression(pattern, Some(where))(pos, Set.empty)

    expr.withOuterScope(Set(varFor("n"))).dependencies should equal(Set(varFor("n")))
  }

  test("should compute dependencies for exists expression with relationship predicate") {
    // MATCH (n)-[r1]->(p1) WHERE EXISTS { (n)-[r2]->(p2) WHERE r1.prop = r2.prop }
    val relChain = RelationshipChain(
      NodePattern(Some(varFor("n")), None, None, None) _,
      RelationshipPattern(Some(varFor("r2")), None, None, None, None, SemanticDirection.OUTGOING) _,
      NodePattern(Some(varFor("p2")), None, None, None) _
    ) _

    val pattern = Pattern(Seq(EveryPath(relChain))) _

    val where = equals(prop(varFor("r1"), "prop"), prop(varFor("r2"), "prop"))

    val expr = ExistsExpression(pattern, Some(where))(pos, Set.empty)

    val outerVariables: Set[LogicalVariable] = Set(varFor("n"), varFor("r1"), varFor("p1"))
    expr.withOuterScope(outerVariables).dependencies should equal(Set(varFor("n"), varFor("r1")))
  }

  test("should compute dependencies for count expression with node predicate") {
    // COUNT { (n)-[r]->(p) WHERE n.prop = p.prop }
    val pattern = RelationshipChain(
      NodePattern(Some(varFor("n")), None, None, None) _,
      RelationshipPattern(Some(varFor("r")), None, None, None, None, SemanticDirection.OUTGOING) _,
      NodePattern(Some(varFor("p")), None, None, None) _
    ) _

    val where = equals(prop(varFor("n"), "prop"), prop(varFor("p"), "prop"))

    val expr = CountExpression(pattern, Some(where))(pos, Set.empty)

    expr.withOuterScope(Set(varFor("n"))).dependencies should equal(Set(varFor("n")))
  }

  test("should compute dependencies for count expression with relationship predicate") {
    // WHERE COUNT { (n)-[r2]->(p2) WHERE r1.prop = r2.prop }
    val pattern = RelationshipChain(
      NodePattern(Some(varFor("n")), None, None, None) _,
      RelationshipPattern(Some(varFor("r2")), None, None, None, None, SemanticDirection.OUTGOING) _,
      NodePattern(Some(varFor("p2")), None, None, None) _
    ) _

    val where = equals(prop(varFor("r1"), "prop"), prop(varFor("r2"), "prop"))

    val expr = CountExpression(pattern, Some(where))(pos, Set.empty)

    val outerVariables: Set[LogicalVariable] = Set(varFor("n"), varFor("r1"), varFor("p1"))
    expr.withOuterScope(outerVariables).dependencies should equal(Set(varFor("n"), varFor("r1")))
  }

  test("should compute inputs of composite expressions") {
    val identA = varFor("a")
    val identB = varFor("b")
    val lit1 = literalInt(1)
    val subExpr = subtract(lit1, identB)
    val addExpr = add(identA, subExpr)

    IdentityMap(addExpr.inputs: _*) should equal(IdentityMap(
      identA -> Set.empty,
      identB -> Set.empty,
      lit1 -> Set.empty,
      subExpr -> Set.empty,
      addExpr -> Set.empty
    ))
  }

  test("should compute inputs for filtering expressions") {
    // given
    val pat = PatternExpression(RelationshipsPattern(
      RelationshipChain(
        NodePattern(Some(varFor("n")), None, None, None) _,
        RelationshipPattern(None, None, None, None, None, SemanticDirection.OUTGOING) _,
        NodePattern(Some(varFor("k")), None, None, None) _
      ) _
    ) _)(Set.empty)

    val callNodes: Expression = function("nodes", varFor("x"))
    val callHead: Expression = function("head", callNodes)

    // [x IN (n)-->(k) | head(nodes(x)) ]
    val expr: Expression = listComprehension(
      varFor("x"),
      pat,
      None,
      Some(callHead)
    )

    // when
    val inputs = IdentityMap(expr.inputs: _*)

    // then
    inputs(callNodes) should equal(Set(varFor("x")))
    inputs(callHead) should equal(Set(varFor("x")))
    inputs(expr) should equal(Set.empty)
    inputs(pat) should equal(Set.empty)
  }
}
