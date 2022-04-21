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
package org.opencypher.v9_0.rewriting

import org.opencypher.v9_0.expressions.Expression
import org.opencypher.v9_0.rewriting.rewriters.Anonymizer
import org.opencypher.v9_0.rewriting.rewriters.anonymizeQuery
import org.opencypher.v9_0.util.OpenCypherExceptionFactory.SyntaxException
import org.opencypher.v9_0.util.Rewriter
import org.opencypher.v9_0.util.test_helpers.CypherFunSuite

class AnonymizeQueryTest extends CypherFunSuite with RewriteTest {

  private val anonymizer = new Anonymizer {
    override def label(name: String): String = "x" + name
    override def relationshipType(name: String): String = "x" + name
    override def labelOrRelationshipType(name: String): String = "x" + name
    override def propertyKey(name: String): String = "X" + name
    override def variable(name: String): String = "X" + name

    override def unaliasedReturnItemName(anonymizedExpression: Expression, input: String): String =
      prettifier.expr(anonymizedExpression)
    override def parameter(name: String): String = "X" + name
    override def literal(value: String): String = s"string[$value]"
  }

  val rewriterUnderTest: Rewriter = anonymizeQuery(anonymizer)

  test("variable") {
    assertRewrite("RETURN 1 AS r", "RETURN 1 AS Xr")
    assertRewrite("WITH 1 AS k RETURN (k + k) - k AS r", "WITH 1 AS Xk RETURN (Xk + Xk) - Xk AS Xr")
  }

  test("return item") {
    assertRewrite("WITH 1 AS k RETURN k + k", "WITH 1 AS Xk RETURN Xk + Xk")
  }

  test("label") {
    assertRewrite("MATCH (:LABEL) RETURN count(*)", "MATCH (:xLABEL) RETURN count(*)")
    assertRewrite("MATCH (:A:B) RETURN count(*)", "MATCH (:xA:xB) RETURN count(*)")
    assertRewrite("MATCH (:A)--(:B) RETURN count(*)", "MATCH (:xA)--(:xB) RETURN count(*)")
    assertRewrite("MATCH (:A)-[r*1..5]-(:B) RETURN count(*)", "MATCH (:xA)-[Xr*1..5]-(:xB) RETURN count(*)")
    assertRewrite("MATCH (n) SET n:LABEL", "MATCH (Xn) SET Xn:xLABEL")
  }

  test("relationship type") {
    assertRewrite("MATCH ()-[:R]-() RETURN count(*)", "MATCH ()-[:xR]-() RETURN count(*)")
    assertRewrite("MATCH ()-[:R*2..4]-() RETURN count(*)", "MATCH ()-[:xR*2..4]-() RETURN count(*)")
    assertRewrite(
      "MATCH shortestPath(()-[:R*2..4]-()) RETURN count(*)",
      "MATCH shortestPath(()-[:xR*2..4]-()) RETURN count(*)"
    )
    assertRewrite("MATCH ()-[r]-() SET r:TYPE", "MATCH ()-[Xr]-() SET Xr:xTYPE")
  }

  test("label or relationship type") {
    assertRewrite(
      "MATCH (n)-[r]->() UNWIND [n, r] AS x WITH x WHERE x:A RETURN n",
      "MATCH (Xn)-[Xr]->() UNWIND [Xn, Xr] AS Xx WITH Xx WHERE Xx:xA RETURN Xn"
    )
    assertRewrite(
      "MATCH (n)-[r]->() UNWIND [n, r] AS x WITH x WHERE x:A:B RETURN n",
      "MATCH (Xn)-[Xr]->() UNWIND [Xn, Xr] AS Xx WITH Xx WHERE Xx:xA:xB RETURN Xn"
    )
  }

  test("label expression") {
    assertRewrite("MATCH (n:A&(B|!C)) RETURN n", "MATCH (Xn:xA&(xB|!xC)) RETURN Xn")
    assertRewrite("MATCH (n) WHERE n:A&(B|!C) RETURN n", "MATCH (Xn) WHERE Xn:xA&(xB|!xC) RETURN Xn")
  }

  test("relationship type expression") {
    val relationshipTypExpressionsImplemented = false

    // This is to catch the eyes of relationship type expression implementers.
    // When this test fails, keep only asserts in "else"-clause.
    if (!relationshipTypExpressionsImplemented) {
      assertThrows[SyntaxException](parseForRewriting("MATCH ()-[r:A&B]->() RETURN r"))
    } else {
      assertRewrite("MATCH ()-[r:A&(B|!C)]->() RETURN n", "MATCH ()-[Xr:xA&(xB|!xC)]->() RETURN Xr")
      assertRewrite("MATCH ()-[r]->() WHERE r:A&(B|!C) RETURN r", "MATCH ()-[r]->() WHERE Xr:xA&(xB|!xC) RETURN Xr")
    }
  }

  test("property key") {
    assertRewrite("MATCH ({p: 2}) RETURN count(*)", "MATCH ({Xp: 2}) RETURN count(*)")
    assertRewrite("MATCH (n) SET p.n = 2", "MATCH (Xn) SET Xp.Xn = 2")
    assertRewrite("MATCH (n) WHERE p.n > 2 RETURN 1", "MATCH (Xn) WHERE Xp.Xn > 2 RETURN 1")
  }

  test("parameter") {
    assertRewrite("RETURN $param1 AS p1, $param2 AS p2", "RETURN $Xparam1 AS Xp1, $Xparam2 AS Xp2")
  }

  test("literals") {
    assertRewrite("RETURN \"hello\"", "RETURN \"string[hello]\"")

  }
}
