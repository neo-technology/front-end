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

import org.opencypher.v9_0.expressions.EveryPath
import org.opencypher.v9_0.expressions.Pattern
import org.opencypher.v9_0.expressions.RelationshipChain
import org.opencypher.v9_0.expressions.RelationshipPattern
import org.opencypher.v9_0.expressions.SemanticDirection
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class ClauseTest extends CypherFunSuite with AstConstructionTestSupport {

  test("containsLabelOrRelTypePredicate with label in where clause") {
    // MATCH (n) WHERE n:N
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(nodePat(Some("n")))))(pos),
      hints = Seq.empty,
      Some(Where(
        hasLabels("n", "N")
      )(pos))
    )(pos)
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(true)
    `match`.containsLabelOrRelTypePredicate("n", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "N") should be(false)
  }

  test("containsLabelOrRelTypePredicate with inlined label") {
    // MATCH (n) WHERE n:N
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(nodePat(Some("n"), Some(labelAtom("N"))))))(pos),
      hints = Seq.empty,
      where = None
    )(pos)
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(true)
    `match`.containsLabelOrRelTypePredicate("n", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "N") should be(false)
  }

  test("containsLabelOrRelTypePredicate with label in where clause nested in AND") {
    // MATCH (n) WHERE n:N AND n.prop = 1
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(nodePat(Some("n")))))(pos),
      hints = Seq.empty,
      Some(Where(
      and(
        hasLabels("n", "N"),
        propEquality("n", "prop", 1)
      )
      )(pos))
    )(pos)
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(true)
    `match`.containsLabelOrRelTypePredicate("n", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "N") should be(false)
  }

  test("containsLabelOrRelTypePredicate with label in where clause nested in ORs") {
    // MATCH (n) WHERE n:N OR n.prop = 1
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(nodePat(Some("n")))))(pos),
      hints = Seq.empty,
      Some(Where(
        ors(
          hasLabels("n", "N"),
          propEquality("n", "prop", 1)
        )
      )(pos))
    )(pos)
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(true)
    `match`.containsLabelOrRelTypePredicate("n", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "M") should be(false)
    `match`.containsLabelOrRelTypePredicate("m", "N") should be(false)
  }

  test("containsLabelOrRelTypePredicate with rel type in where clause") {
    // MATCH ()-[r]-() WHERE r:R
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(
        RelationshipChain(nodePat(),
        RelationshipPattern(Some(varFor("r")), Seq.empty, None, None, None, SemanticDirection.BOTH)(pos),
        nodePat())(pos)
      )))(pos),
      hints = Seq.empty,
      Some(Where(
        hasTypes("r", "R")
      )(pos))
    )(pos)

    // assert
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("n", "R") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "R") should be(true)
  }

  test("containsLabelOrRelTypePredicate with rel type in where clause checked by labelExpressionPredicate") {
    // MATCH ()-[r]-() WHERE r:R
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(
        RelationshipChain(nodePat(),
          RelationshipPattern(Some(varFor("r")), Seq.empty, None, None, None, SemanticDirection.BOTH)(pos),
          nodePat())(pos)
      )))(pos),
      hints = Seq.empty,
      Some(Where(
        labelExpressionPredicate("r", labelAtom("R"))
      )(pos))
    )(pos)

    // assert
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("n", "R") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "R") should be(true)
  }

  test("containsLabelOrRelTypePredicate with inlined rel type") {
    // MATCH ()-[r:R]-()
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(
        RelationshipChain(nodePat(),
          RelationshipPattern(Some(varFor("r")), Seq(relTypeName("R")), None, None, None, SemanticDirection.BOTH)(pos),
          nodePat())(pos)
      )))(pos),
      hints = Seq.empty,
      None
    )(pos)

    // assert
    `match`.containsLabelOrRelTypePredicate("n", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("n", "R") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "N") should be(false)
    `match`.containsLabelOrRelTypePredicate("r", "R") should be(true)
  }

  test("containsPropertyPredicates with inlined rel property predicate") {
    // MATCH ()-[r:R {prop: 42}]-()
    val `match` = Match(optional = false,
      Pattern(Seq(EveryPath(
        RelationshipChain(nodePat(),
          RelationshipPattern(Some(varFor("r")), Seq(relTypeName("R")), None, Some(mapOfInt("prop" -> 42)), None, SemanticDirection.BOTH)(pos),
          nodePat())(pos)
      )))(pos),
      hints = Seq.empty,
      None
    )(pos)

    // assert
    `match`.containsPropertyPredicates("n", Seq(propName("prop"))) should be(false)
    `match`.containsPropertyPredicates("n", Seq(propName("flop"))) should be(false)
    `match`.containsPropertyPredicates("r", Seq(propName("prop"))) should be(true)
    `match`.containsPropertyPredicates("r", Seq(propName("flop"))) should be(false)
  }
}
