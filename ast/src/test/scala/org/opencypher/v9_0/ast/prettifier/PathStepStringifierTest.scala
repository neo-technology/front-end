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
package org.opencypher.v9_0.ast.prettifier

import org.opencypher.v9_0.ast.AstConstructionTestSupport
import org.opencypher.v9_0.expressions.MultiRelationshipPathStep
import org.opencypher.v9_0.expressions.NilPathStep
import org.opencypher.v9_0.expressions.NodePathStep
import org.opencypher.v9_0.expressions.SemanticDirection.BOTH
import org.opencypher.v9_0.expressions.SemanticDirection.INCOMING
import org.opencypher.v9_0.expressions.SemanticDirection.OUTGOING
import org.opencypher.v9_0.expressions.SingleRelationshipPathStep
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class PathStepStringifierTest extends CypherFunSuite with AstConstructionTestSupport {

  private val expressionStringifier = ExpressionStringifier()
  private val pathStringifier = PathStepStringifier(expressionStringifier)

  test("SingleRelationshipPathStep with outgoing relationship direction") {
    val pathStep = NodePathStep(
      varFor("a"),
      SingleRelationshipPathStep(varFor("b"), OUTGOING, Some(varFor("c")), NilPathStep()(pos))(pos)
    )(pos)

    assert(pathStringifier(pathStep) === "(a)-[b]->(c)")
  }

  test("MultiRelationshipPathStep with incoming relationship direction") {
    val pathStep = NodePathStep(
      varFor("a"),
      MultiRelationshipPathStep(varFor("b"), INCOMING, Some(varFor("c")), NilPathStep()(pos))(pos)
    )(pos)

    assert(pathStringifier(pathStep) === "(a)<-[b*]-(c)")
  }

  test("Multiple relationship path steps") {
    val nextPathStep = SingleRelationshipPathStep(varFor("d"), BOTH, Some(varFor("e")), NilPathStep()(pos))(pos)
    val pathStep = NodePathStep(
      varFor("a"),
      MultiRelationshipPathStep(varFor("b"), OUTGOING, Some(varFor("c")), nextPathStep)(pos)
    )(pos)

    assert(pathStringifier(pathStep) === "(a)-[b*]->(c)-[d]-(e)")
  }
}
