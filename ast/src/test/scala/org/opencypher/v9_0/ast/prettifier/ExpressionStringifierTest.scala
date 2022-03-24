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
import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.expressions.PatternComprehension
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.RelationshipsPattern
import org.opencypher.v9_0.expressions.SemanticDirection.OUTGOING
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ExpressionStringifierTest extends CypherFunSuite with AstConstructionTestSupport {

  private val tests: Seq[(Expression, String)] = Seq(
    (
      PatternComprehension(
        namedPath = None,
        pattern = RelationshipsPattern(RelationshipChain(
          nodePat(Some("u")),
          RelationshipPattern(Some(varFor("r")), List(relTypeName("FOLLOWS")), None, None, None, OUTGOING)(pos),
          nodePat(Some("u2"))
        )(pos))(pos),
        predicate = Some(hasLabels("u2", "User")),
        projection = prop("u2", "id"))(
        pos,
        outerScope = Set(varFor("u.id"), varFor("u")),
        variableToCollectName = "p",
        collectionName = "v"
      ),
      "[(u)-[r:FOLLOWS]->(u2) WHERE u2:User | u2.id]"
    ),
    (
      PatternComprehension(
        namedPath = None,
        pattern = RelationshipsPattern(RelationshipChain(
          nodePat(Some("u")),
          RelationshipPattern(Some(varFor("r")), List(relTypeName("FOLLOWS")), None, None, None, OUTGOING)(pos),
          nodePat(Some("u2"), Some(labelAtom("User")))
        )(pos))(pos),
        predicate = None,
        projection = prop("u2", "id"))(
        pos,
        outerScope = Set(varFor("u.id"), varFor("u")),
        variableToCollectName = "p",
        collectionName = "v"
      ),
      "[(u)-[r:FOLLOWS]->(u2:User) | u2.id]"
    )
  )

  private val stringifier = ExpressionStringifier()

  for (((expr, expectedResult), idx) <- tests.zipWithIndex) {
    test(s"[$idx] should produce $expectedResult") {
      withClue(expr) {
        lazy val stringifiedExpr = stringifier(expr)
        noException should be thrownBy stringifiedExpr
        stringifiedExpr shouldBe expectedResult
      }
    }
  }
}
